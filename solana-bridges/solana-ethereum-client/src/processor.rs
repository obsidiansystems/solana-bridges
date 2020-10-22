#![cfg(feature = "program")]

use crate::{
    eth::*,
    instruction::*,
    parameters::*,
    types::*,
    prove::*,
};

use std::mem;

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

    Ok(match Instruction::unpack(instruction_data)? {
        Instruction::Noop => {},
        Instruction::Initialize(item) => {
            if !account.is_signer {
                info!("Account does not have the correct program id");
                return Err(ProgramError::MissingRequiredSignature);
            }

            guard_sufficient_storage(&account)?;
            let mut raw_data = account.try_borrow_mut_data()?;
            let data = interp_mut(&mut *raw_data);

            match data {
                Storage { height: 0, offset: 0, full: false, .. } => (),
                _ => return Err(CustomError::AlreadyInitialized.to_program_error()),
            };
            if !verify_block(&item.header, None) {
                return Err(CustomError::VerifyHeaderFailed.to_program_error());
            };

            write_new_block(data, &item.header, Some(&item.total_difficulty))?;
        },
        Instruction::NewBlock(header) => {
            guard_sufficient_storage(&account)?;
            let mut raw_data = account.try_borrow_mut_data()?;
            let data = interp_mut(&mut *raw_data);

            let parent = read_prev_block(data)?
                .ok_or(CustomError::BlockNotFound.to_program_error())?;
            if !verify_block(&header, Some(&parent.header)) {
                return Err(CustomError::VerifyHeaderFailed.to_program_error());
            };

            write_new_block(data, &header, None)?;
        },
        Instruction::ProveInclusion(pi) => {
            if account.is_writable {
                return Err(CustomError::WritableHistoryDuringProofCheck.to_program_error());
            }
            guard_sufficient_storage(&account)?;
            let mut raw_data = account.try_borrow_mut_data()?;
            let data = interp(&mut *raw_data);

            let min_h = min_height(data);
            if min_h > pi.height {
                panic!("too old {} {}", min_h, pi.height)
            }
            let max_h = min_h + data.headers.len() as u64;
            if max_h <= pi.height {
                panic!("too new {} {}", max_h, pi.height)
            }
            let offset = lowest_offset(data) + (pi.height - min_h) as usize % data.headers.len();
            let block = read_block(data, offset)?
                .ok_or(CustomError::BlockNotFound.to_program_error())?;
            if hash_header(&block.header, false) != pi.block_hash {
                return Err(CustomError::InvalidProof_BadBlockHash.to_program_error());
            }
            if block.total_difficulty < pi.min_difficulty {
                return Err(CustomError::InvalidProof_TooEasy.to_program_error());
            }
            let expected_root = block.header.receipts_root; // pi.block_hash
            let rlp = Rlp::new(&*pi.proof);
            let proof = rlp.iter().map(|rlp| rlp.data());
            verify_trie_proof(expected_root, &*pi.key, proof, &*pi.expected_value)
                .map_err(|_| CustomError::InvalidProof_BadMerkle.to_program_error())?;
        },
    })
}

#[inline]
pub fn interp(raw_data: &[u8]) -> &Storage {
    let raw_len = raw_data.len();
    let block_len = raw_data[BLOCKS_OFFSET..].len() / mem::size_of::<RingItem>();
    let hacked_data = &raw_data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &Storage = unsafe { std::mem::transmute(hacked_data) };
    // because no stride != size
    debug_assert!(std::mem::size_of_val(res) <= (raw_len + STORAGE_ALIGN - 1 / STORAGE_ALIGN) * STORAGE_ALIGN);
    debug_assert_eq!(res.headers.len(), block_len);
    res
}

#[inline]
pub fn interp_mut(raw_data: &mut [u8]) -> &mut Storage {
    let raw_len = raw_data.len();
    let block_len = raw_data[BLOCKS_OFFSET..].len() / mem::size_of::<RingItem>();
    let hacked_data = &mut raw_data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &mut Storage = unsafe { std::mem::transmute(hacked_data) };
    // because no stride != size
    debug_assert!(std::mem::size_of_val(res) <= (raw_len + STORAGE_ALIGN - 1 / STORAGE_ALIGN) * STORAGE_ALIGN);
    debug_assert_eq!(res.headers.len(), block_len);
    res
}

pub fn min_height(data: &Storage) -> u64 {
    let len = data.headers.len();
    match *data {
        Storage { full: false, offset, .. } => data.height - offset as u64 + 1,
        Storage { full: true, .. } => data.height - len as u64 + 1,
    }
}

pub fn lowest_offset(data: &Storage) -> usize {
    match *data {
        Storage { full: false, .. } => 0,
        Storage { full: true, offset, .. } => offset,
    }
}

pub fn read_block<'a>(data: &'a Storage, idx: usize) -> Result<Option<&'a RingItem>, ProgramError> {
    let len = data.headers.len();
    match *data {
        Storage { full: false, offset, .. } if idx < offset
            => (),
        Storage { full: true, .. } if idx < len
            => (),
        _ => return Ok(None),
    };
    assert!(data.height != 0);
    let ref header = data.headers[idx];
    Ok(Some(header))
}

pub fn read_prev_block<'a>(data: &'a Storage) -> Result<Option<&'a RingItem>, ProgramError> {
    let len = data.headers.len();
    read_block(data, (data.offset + (len - 1)) % len)
}

pub fn write_new_block(data: &mut Storage, header: &BlockHeader, old_total_difficulty_opt: Option<&U256>) -> Result<(), ProgramError> {
    let old_offset = data.offset;

    const ZERO: U256 = U256([0; 4]);

    let old_total_difficulty: &_ = match old_total_difficulty_opt {
        Some(d) => d,
        None => match read_prev_block(data)? {
            None => &ZERO,
            Some(prev_item) => &prev_item.total_difficulty,
        },
    };

    data.headers[old_offset] = RingItem {
        header: header.clone(),
        total_difficulty: old_total_difficulty + header.difficulty,
    };

    data.height = header.number;
    data.offset = (old_offset + 1) % data.headers.len();
    data.full |= data.offset <= old_offset;

    return Ok(());
}


fn guard_sufficient_storage(account: &AccountInfo) -> Result<(), ProgramError> {
    if MIN_BUF_SIZE > account.data_len() {
        info!("Account data length too small for holding state");
        return Err(ProgramError::AccountDataTooSmall);
    }
    return Ok(());
}
