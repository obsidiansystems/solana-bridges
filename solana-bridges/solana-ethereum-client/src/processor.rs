#![cfg(feature = "program")]

use crate::{
    eth::*,
    instruction::*,
    types::*,
};

use arrayref::{array_mut_ref, array_ref};

use solana_sdk::{
    account_info::{next_account_info, AccountInfo},
    entrypoint_deprecated::ProgramResult,
    info,
    program_error::ProgramError,
    program_pack::{Pack},
    pubkey::Pubkey,
};
use std::mem;

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

    guard_sufficient_storage(&account)?;

    match Instruction::unpack(instruction_data)? {
        Instruction::Noop => return Ok(()),
        Instruction::Initialize(block) => {
            if !verify_block(&block.header, None) {
                return Err(CustomError::VerifyHeaderFailed.to_program_error());
            };

            write_new_block(account, block.header)?;
        }
        Instruction::NewBlock(block) => match read_prev_block(account) {
            Err(e) => return Err(e),
            Ok(parent) => {
                if !verify_block(&block.header, Some(&parent)) {
                    return Err(CustomError::VerifyHeaderFailed.to_program_error());
                };
                write_new_block(account, block.header)?;
            }
        }
    };

    return Ok(());
}

pub fn read_prev_block(account: &AccountInfo) -> Result<BlockHeader, ProgramError> {
    let data = account.try_borrow_mut_data()?;

    const COUNT_SIZE: usize = mem::size_of::<usize>();
    let count_src = array_ref![data, 0, COUNT_SIZE];

    match usize::from_le_bytes(*count_src) {
        0 => return Err(CustomError::NoParentBlock.to_program_error()),
        count => {
            let header_src = array_ref![data, COUNT_SIZE + BlockHeader::LEN * (count - 1), BlockHeader::LEN];
            let header = BlockHeader::unpack_from_slice(header_src)?;
            return Ok(header);
        }
    }
}

pub fn write_new_block(account: &AccountInfo, header: BlockHeader) -> Result<(), ProgramError> {
    let mut data = account.try_borrow_mut_data()?;

    const COUNT_SIZE: usize = mem::size_of::<usize>();
    let count_src = array_mut_ref![data, 0, COUNT_SIZE];
    let count = usize::from_le_bytes(*count_src);
    *count_src = (count + 1).to_le_bytes();

    let header_dst = array_mut_ref![data, COUNT_SIZE + BlockHeader::LEN * count, BlockHeader::LEN];
    header.pack_into_slice(header_dst);
    return Ok(());
}

pub fn account_deserialize_data (account: &AccountInfo) -> Result<State, ProgramError> {
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
    if State::LEN > account.data_len() {
        info!("Account data length too small for holding state");
        return Err(ProgramError::AccountDataTooSmall);
    }
    return Ok(());
}
