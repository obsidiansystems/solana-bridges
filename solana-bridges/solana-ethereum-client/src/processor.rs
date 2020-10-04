#![cfg(feature = "program")]

use crate::{
    eth::{State, initialize, new_block},
    instruction::Instruction,
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

    guard_sufficient_storage(&account)?;

    let new_state = match Instruction::unpack(instruction_data)? {
        Instruction::Noop => return Ok(()),
        Instruction::Initialize(header) => initialize(header),
        Instruction::NewBlock(header) => new_block(account_deserialize_data(account).map_err(|_| ProgramError::InvalidAccountData)?, header),
    };

    account_serialize_data(account, &new_state?)?;

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
