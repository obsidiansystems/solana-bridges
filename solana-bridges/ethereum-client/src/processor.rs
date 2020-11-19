#![cfg(feature = "program")]

use ethereum_types::U256;

use rlp::Rlp;

use solana_program::info;
use solana_sdk::{
    account_info::{next_account_info, AccountInfo},
    entrypoint_deprecated::ProgramResult,
    program_error::ProgramError,
    pubkey::Pubkey,
};

use crate::{
    eth::*,
    instruction::*,
    ledger_ring_buffer::*,
    pow_proof::*,
    prove::*,
    types::*,
};

pub fn process_instruction<'a>(
    program_id: &Pubkey,
    accounts: &[AccountInfo<'a>],
    instruction_data: &[u8],
) -> ProgramResult {
    info!("Ethereum light client entrypoint");

    let accounts_iter = &mut accounts.iter();
    let account = next_account_info(accounts_iter)?;

    if account.owner != program_id {
        info!("Account does not have the correct program id");
        return Err(ProgramError::IncorrectProgramId);
    }

    {
        let raw_data = account.try_borrow_data()?;
        let data = interp(&*raw_data)?;
        if data.dead {
            return Err(CustomError::ContractIsDead.to_program_error());
        }
    }


    let instr = Instruction::unpack(instruction_data)?;
    //println!("{:#?}", instr);

    Ok(match instr {
        Instruction::Noop => {}
        Instruction::Initialize(item) => {
            if !account.is_signer {
                info!("Account does not have the correct program id");
                return Err(ProgramError::MissingRequiredSignature);
            }

            let mut raw_data = account.try_borrow_mut_data()?;
            let ref mut data = *interp_mut(&mut *raw_data)?;

            match data {
                Storage {
                    height: 0,
                    offset: 0,
                    full: false,
                    ..
                } => (),
                _ => return Err(CustomError::AlreadyInitialized.to_program_error()),
            };
            verify_block(&item.header, None).map_err(CustomError::to_program_error)?;

            write_new_block(data, &item.header, Some(&item.total_difficulty))?;
        }
        Instruction::NewBlock(header) => {
            let mut raw_data = account.try_borrow_mut_data()?;
            let ref mut data = *interp_mut(&mut *raw_data)?;

            let parent =
                read_prev_block(data)?.ok_or(CustomError::BlockNotFound.to_program_error())?;
            verify_block(&header, Some(&parent.header)).map_err(CustomError::to_program_error)?;

            write_new_block(data, &header, None)?;
        }
        Instruction::ProvidePowElement(ppe) => {
            let mut raw_data = account.try_borrow_mut_data()?;
            let ref mut data = *interp_mut(&mut *raw_data)?;
            //println!("{} {:?}", ppe.chunk_offset, data.ethash_elements);
            if ppe.height != data.height {
                return Err(CustomError::EthashElementsForWrongBlock.to_program_error())
            }
            let mut bit_vec = data.ethash_elements;
            let block = read_prev_block_mut(data)?
                .ok_or(CustomError::BlockNotFound.to_program_error())?;
            for i in 0..ProvidePowElement::ETHASH_ELEMENTS_PER_INSTRUCTION {
                let offset = ppe.chunk_offset * ProvidePowElement::ETHASH_ELEMENTS_PER_INSTRUCTION;
                let new_value = ppe.elements[i as usize];

                match block.elements[offset + i].value {
                    _ if bit_vec.get_has_chunk(ppe.chunk_offset) => block.elements[offset + i].value = new_value,
                    h if h == new_value => (),
                    _ => return Err(CustomError::EthashElementRewriting.to_program_error()),
                }
            }
            bit_vec.set_has_chunk(ppe.chunk_offset);
            if bit_vec != ElementChunkSet::READY_FOR_BLOCK {
                // keep waiting for elements
            } else {
                // We have all the elements now, verify PoW
                let pow_valid = verify_pow_indexes(block);
                if !pow_valid {
                    return Err(CustomError::VerifyHeaderFailed_InvalidProofOfWork
                               .to_program_error());
                }
            }
            data.ethash_elements = bit_vec;
        }

        Instruction::ProveInclusion(pi) => {
            if account.is_writable {
                return Err(CustomError::WritableHistoryDuringProofCheck.to_program_error());
            }
            let raw_data = account.try_borrow_data()?;
            let data = interp(&*raw_data)?;

            let block = find_block(&data, pi.height)?;
            if &hash_header(&block.header, false) != &*pi.block_hash {
                return Err(CustomError::InvalidProof_BadBlockHash.to_program_error());
            }

            if &block.total_difficulty < &*pi.min_difficulty {
                return Err(CustomError::InvalidProof_TooEasy.to_program_error());
            }
            let expected_root = block.header.receipts_root; // pi.block_hash
            let rlp = Rlp::new(&*pi.proof);
            let proof = rlp.iter().map(|rlp| rlp.data());
            verify_trie_proof(expected_root, &*pi.key, proof, &*pi.expected_value)
                .map_err(|_| CustomError::InvalidProof_BadMerkle.to_program_error())?;
        }
        Instruction::Challenge(challenge) => {
            let mut raw_data = account.try_borrow_mut_data()?;
            let data = interp_mut(&mut *raw_data)?;

            let block = find_block(&data, challenge.height)?;

            if &hash_header(&block.header, false) != &*challenge.block_hash {
                return Err(CustomError::InvalidChallenge_BadBlockHash.to_program_error());
            }

            if challenge.element_index >= 64 {
                panic!("element pair index must be between 0 and 64")
            }

            let challenged_0 = &block.elements[challenge.element_index * 2];
            let challenged_1 = &block.elements[challenge.element_index * 2 + 1];

            // Make sure addresses are in the form (n, n + 1) (failure would
            // indicate contract bug not bad input.)
            if challenged_0.address + 1 != challenged_1.address {
                panic!("non-consecutive addresses")
            }

            let found = Box::new(ElementPair {
                e0: challenged_0.value,
                e1: challenged_1.value,
            });

            if challenge.element_pair == found {
                return Err(CustomError::InvalidChallenge_SameElement.to_program_error());
            }

            let wanted_merkle_root = get_wanted_merkle_root(challenge.height);

            let got_merkle_root = apply_pow_element_merkle_proof(
                &challenge.element_pair,
                &*challenge.merkle_spine,
                challenged_0.address,
            );

            if got_merkle_root != wanted_merkle_root {
                return Err(CustomError::InvalidChallenge_BadMerkleRoot.to_program_error());
            }

            let dst_account = next_account_info(accounts_iter)?;

            give_bounty_to_challenger(account, dst_account)?;

            data.dead = true;
        }
    })
}

pub fn find_block<'a>(data: &'a Storage, height: u64) -> Result<&'a RingItem, ProgramError> {
    let min_h = min_height(data);
    if min_h > height {
        //panic!("too old {} {}", min_h, height)
        return Err(CustomError::BlockNotFound.to_program_error());
    }
    let mut max_h = data.height;
    if data.ethash_elements != ElementChunkSet::READY_FOR_BLOCK {
        // last block doesn't have all it's elements
        max_h -= 1;
    }
    if max_h < height {
        //panic!("too new {} {}", max_h, height);
        return Err(CustomError::BlockNotFound.to_program_error());
    }
    let offset = lowest_offset(data) + (height - min_h) as usize % data.headers.len();

    // TODO: Check that we've actually run the PoW for this one

    read_block(data, offset)?.ok_or(CustomError::BlockNotFound.to_program_error())
}

pub fn give_bounty_to_challenger(src_account: &AccountInfo, dst_account: &AccountInfo) -> ProgramResult {
    **dst_account.lamports.borrow_mut() += src_account.lamports();
    **src_account.lamports.borrow_mut() = 0;
    Ok(())
}

pub fn write_new_block(
    data: &mut Storage,
    header: &BlockHeader,
    old_total_difficulty_opt: Option<&U256>,
) -> Result<(), ProgramError> {
    if data.ethash_elements != ElementChunkSet::READY_FOR_BLOCK {
        panic!("expected PoW element for previous block, but we're trying to write a new block")
    }
    write_new_block_unvalidated(data, header, old_total_difficulty_opt)?;
    data.ethash_elements = ElementChunkSet::NEED_ALL_ELEMS;
    Ok(())
}
