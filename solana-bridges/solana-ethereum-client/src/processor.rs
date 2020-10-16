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
            if !account.is_signer {
                info!("Account does not have the correct program id");
                return Err(ProgramError::MissingRequiredSignature);
            }
            {
                let raw_data = account.try_borrow_data()?;
                let data = interp(&*raw_data);
                match data {
                    Storage { height: 0, offset: 0, full: false, .. } => (),
                    _ => return Err(CustomError::AlreadyInitialized.to_program_error()),
                };
            }
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

    Ok(())
}

#[inline]
pub fn interp(raw_data: &[u8]) -> &Storage {
    let raw_len = raw_data.len();
    let block_len = raw_data[BLOCKS_OFFSET..].len() / BlockHeader::LEN;
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
    let block_len = raw_data[BLOCKS_OFFSET..].len() / BlockHeader::LEN;
    let hacked_data = &mut raw_data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &mut Storage = unsafe { std::mem::transmute(hacked_data) };
    // because no stride != size
    debug_assert!(std::mem::size_of_val(res) <= (raw_len + STORAGE_ALIGN - 1 / STORAGE_ALIGN) * STORAGE_ALIGN);
    debug_assert_eq!(res.headers.len(), block_len);
    res
}

pub fn read_prev_block(account: &AccountInfo) -> Result<BlockHeader, ProgramError> {
    guard_sufficient_storage(&account)?;
    let mut raw_data = account.try_borrow_mut_data()?;
    let data = interp_mut(&mut *raw_data);
    match data {
        Storage { offset: 0, full: false, .. } =>
            return Err(CustomError::NoParentBlock.to_program_error()),
        _ => (),
    };
    assert!(data.height != 0);
    let len = data.headers.len();
    let ref header_src = data.headers[(data.offset + (len - 1)) % len];
    let header = BlockHeader::unpack_from_slice(header_src)?;
    Ok(header)
}

pub fn write_new_block(account: &AccountInfo, header: BlockHeader) -> Result<(), ProgramError> {
    guard_sufficient_storage(&account)?;
    let mut raw_data = account.try_borrow_mut_data()?;
    let data = interp_mut(&mut *raw_data);

    let old_offset = data.offset;

    header.pack_into_slice(&mut data.headers[old_offset]);

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
