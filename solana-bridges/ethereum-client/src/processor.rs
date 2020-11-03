#![cfg(feature = "program")]

use crate::{eth::*, instruction::*, parameters::*, prove::*, types::*};

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

            write_new_block(
                data,
                &item.header,
                Some(&item.total_difficulty),
                &item.elements,
            )?;
        }
        Instruction::NewBlock(nb) => {
            let NewBlock { header, elements } = *nb;
            let mut raw_data = account.try_borrow_mut_data()?;
            let ref mut data = *interp_mut(&mut *raw_data)?;

            let parent =
                read_prev_block(data)?.ok_or(CustomError::BlockNotFound.to_program_error())?;
            verify_block(&header, Some(&parent.header)).map_err(CustomError::to_program_error)?;

            write_new_block(data, &header, None, &elements)?;
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
        Instruction::Challenge(_c) => {
        }
    })
}

pub fn verify_pow_indexes(header: &BlockHeader, elems: &[[u8; 64]; 128]) -> bool {
    let mut iter = elems.iter().map(H512::from);
    verify_pow(&header, |_| {
        iter.next().unwrap()
    })
}

pub fn write_new_block(
    data: &mut Storage,
    header: &BlockHeader,
    old_total_difficulty_opt: Option<&U256>,
    elems: &AccessedElements,
) -> Result<(), ProgramError> {
    let pow_valid = verify_pow(&header, |addr| {
        // reshape; TODO use safe transmute for this
        let elems_flat: &[(u32, H512); 128] = unsafe { std::mem::transmute(&elems.0) };
        // if element is missing just use index 0. Will catch later when proof
        // of work is invalid.
        let idx = elems_flat.binary_search_by_key(&addr, |(k, _)| *k)
            // temporarily make addr not 0 for sake of debugging
            .ok().unwrap_or(addr as usize);
        elems_flat[idx].1
    });
    if !pow_valid {
        return Err(CustomError::VerifyHeaderFailed_InvalidProofOfWork.to_program_error());
    }
    write_new_block_unvalidated(data, header, old_total_difficulty_opt, elems)
}
