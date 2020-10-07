#![cfg(feature = "program")]

use crate::{
    eth::*,
    instruction::*,
    parameters::*,
    types::*,
};

use solana_sdk::{
    account_info::{next_account_info, AccountInfo},
    entrypoint_deprecated::ProgramResult,
    info,
    program_error::ProgramError,
    program_pack::{Pack},
    pubkey::Pubkey,
};

pub fn process_instruction<'a>(
    program_id: &Pubkey,
    accounts: &'a [AccountInfo<'a>],
    instruction_data: &[u8],
) -> ProgramResult {
    info!("Ethereum light client entrypoint");

    let accounts_iter = &mut accounts.iter();
    let account = next_account_info(accounts_iter)?;

    if account.owner != program_id {
        info!("Account does not have the correct program id");
        return Err(ProgramError::IncorrectProgramId);
    }

    match Instruction::unpack(instruction_data)? {
        Instruction::Noop => return Ok(()),
        Instruction::Initialize(block_header) => {
            if !verify_block(&block_header, None) {
                return Err(CustomError::VerifyHeaderFailed.to_program_error());
            };

            write_new_block(account, block_header)?;
        }
        Instruction::NewBlock(block_header) => match read_prev_block(account) {
            Err(e) => return Err(e),
            Ok(parent) => {
                if !verify_block(&block_header, Some(&parent)) {
                    return Err(CustomError::VerifyHeaderFailed.to_program_error());
                };
                write_new_block(account, block_header)?;
            }
        }
    };

    return Ok(());
}

pub fn interp(mut data: &[u8]) -> &Storage {
    let block_len = data[BLOCKS_OFFSET..].len() / BlockHeader::LEN;
    data = &data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &Storage = unsafe { std::mem::transmute(data) };
    debug_assert_eq!(res.headers.len(), block_len);
    res
}

pub fn interp_mut(mut data: &mut [u8]) -> &mut Storage {
    let block_len = data[BLOCKS_OFFSET..].len() / BlockHeader::LEN;
    data = &mut data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &mut Storage = unsafe { std::mem::transmute(data) };
    debug_assert_eq!(res.headers.len(), block_len);
    res
}

/// This is a number in [0, len * 2).
///
/// n in [0, len) means we've filled:
///
///   [height - n + 1, ... height]
///
/// n in [len, len * 2) means we've filled:
///
///   [height - n + 1 ... height, height - ring_len + 1 ... height - n]
///
pub fn normalize_count(data: &Storage, new_count: usize) -> usize {
    let len = data.headers.len();
    let wrapped = new_count >= len;
    new_count % len + if wrapped { len } else { 0 }
}

pub fn read_prev_block(account: &AccountInfo) -> Result<BlockHeader, ProgramError> {
    guard_sufficient_storage(&account)?;
    let mut raw_data = account.try_borrow_mut_data()?;
    let data = interp_mut(&mut *raw_data);

    match data.count {
        0 => return Err(CustomError::NoParentBlock.to_program_error()),
        count => {
            let len = data.headers.len();
            let ref header_src = data.headers[(count - 1) as usize % len];
            let header = BlockHeader::unpack_from_slice(header_src)?;
            return Ok(header);
        }
    }
}

pub fn write_new_block(account: &AccountInfo, header: BlockHeader) -> Result<(), ProgramError> {
    guard_sufficient_storage(&account)?;
    let mut raw_data = account.try_borrow_mut_data()?;
    let data = interp_mut(&mut *raw_data);

    let count = data.count;
    data.count = normalize_count(data, count + 1);
    data.height = header.number;

    let len = data.headers.len();
    header.pack_into_slice(&mut data.headers[count as usize % len]);
    return Ok(());
}

pub fn account_deserialize_data (account: &AccountInfo) -> Result<State, ProgramError> {
    guard_sufficient_storage(&account)?;
    let data = account.try_borrow_mut_data()?;
    return State::unpack_from_slice(&data);
}

pub fn account_serialize_data (account: &AccountInfo, state: &State) -> Result<(), ProgramError> {
    guard_sufficient_storage(&account)?;
    let mut data = account.data.borrow_mut();
    state.pack_into_slice(&mut data);
    return Ok(())
}

fn guard_sufficient_storage(account: &AccountInfo) -> Result<(), ProgramError> {
    if MIN_BUF_SIZE > account.data_len() {
        info!("Account data length too small for holding state");
        return Err(ProgramError::AccountDataTooSmall);
    }
    return Ok(());
}
