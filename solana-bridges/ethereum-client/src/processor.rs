#![cfg(feature = "program")]

pub use ethereum_types::{H128};

use crate::{
    eth::*,
    instruction::*,
    ledger_ring_buffer::*,
    pow_proof::*,
    prove::*,
    types::*,
};

use rlp::Rlp;

use solana_sdk::{
    account_info::{next_account_info, AccountInfo},
    entrypoint_deprecated::ProgramResult,
    info,
    program_error::ProgramError,
    pubkey::Pubkey,
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
        Instruction::NewBlock(nb) => {
            let NewBlock { header } = *nb;
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
            data.ethash_elements = match data.ethash_elements {
                None => panic!("Waiting for new block, cannot accept another PoW element."),
                Some(n) => {
                    let parent = read_prev_block_mut(data)?
                        .ok_or(CustomError::BlockNotFound.to_program_error())?;
                    parent.elements.0[(n / 4) as usize][(n % 4) as usize].value = *ppe.element;
                    if n < 127 {
                        // do nothing but increment
                        Some(n + 1)
                    } else {
                        // We have all the blocks now, verify PoW and write addresses
                        let pow_valid = verify_pow_indexes(parent);
                        if !pow_valid {
                            return Err(CustomError::VerifyHeaderFailed_InvalidProofOfWork
                                .to_program_error());
                        }

                        // indicate we are ready for new address
                        None
                    }
                },
            }
        }
        Instruction::ProveInclusion(pi) => {
            if account.is_writable {
                return Err(CustomError::WritableHistoryDuringProofCheck.to_program_error());
            }
            let raw_data = account.try_borrow_data()?;
            let data = interp(&*raw_data)?;

            let min_h = min_height(data);
            if min_h > pi.height {
                panic!("too old {} {}", min_h, pi.height)
            }
            let max_h = min_h + data.headers.len() as u64;
            if max_h <= pi.height {
                panic!("too new {} {}", max_h, pi.height)
            }
            let offset = lowest_offset(data) + (pi.height - min_h) as usize % data.headers.len();
            let block =
                read_block(data, offset)?.ok_or(CustomError::BlockNotFound.to_program_error())?;
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
            if account.is_writable {
                return Err(CustomError::WritableHistoryDuringProofCheck.to_program_error());
            }
            let raw_data = account.try_borrow_data()?;
            let data = interp(&*raw_data)?;

            let min_h = min_height(data);
            if min_h > challenge.height {
                panic!("too old {} {}", min_h, challenge.height)
            }
            let max_h = min_h + data.headers.len() as u64;
            if max_h <= challenge.height {
                panic!("too new {} {}", max_h, challenge.height)
            }

            // Check that we've actually run the PoW for this one

            let offset =
                lowest_offset(data) + (challenge.height - min_h) as usize % data.headers.len();
            let block =
                read_block(data, offset)?.ok_or(CustomError::BlockNotFound.to_program_error())?;
            if &hash_header(&block.header, false) != &*challenge.block_hash {
                return Err(CustomError::InvalidChallenge_BadBlockHash.to_program_error());
            }

            if challenge.element_index >= 643 {
                panic!("element pair index must be between 0 and 64")
            }

            // Make sure addresses are in the form (n, n + 1) (failure would
            // indicate contract bug not bad input.)
            block.ethash_elements;


            let merkel_root = get_current_epoch_merkel_root();

            let element_being_challenged = get_pow_element(block, challenge.element_index);

            if(elements_match(element_being_challenged, )) {
                panic!("challenger matches challengee") //TODO: Return an actual error
            }

            // Verify that the element provided in the proof doesn't match the one we have (invalid challenge if it matches)

            // Start at the element they provided and go up the proof until you get to the epoch root
        }
    })
}

pub fn get_current_epoch_merkel_root() -> H128 {
    unimplemented!()
}

pub fn get_pow_element(ri: &RingItem, i: u8) -> &AccessedElement {
    unimplemented!()
}

pub fn elements_match(a: &AccessedElement, b: &AccessedElement) -> bool {
    unimplemented!()
}

pub fn verify_pow_indexes(ri: &mut RingItem) -> bool {
    let mut iter = ri.elements.0.iter_mut().flat_map(|x| x.iter_mut());
    verify_pow(&ri.header, |wanted_addr| {
        let a = iter.next().unwrap();
        // Set for challengers, now that we know what it is.
        a.address = wanted_addr;
        a.value
    })
}

pub fn write_new_block(
    data: &mut Storage,
    header: &BlockHeader,
    old_total_difficulty_opt: Option<&U256>,
) -> Result<(), ProgramError> {
    if data.ethash_elements.is_some() {
        panic!("expected PoW element for previous block, but we're trying to write a new block")
    }
    write_new_block_unvalidated(data, header, old_total_difficulty_opt)?;
    data.ethash_elements = Some(0);
    Ok(())
}
