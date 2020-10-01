#![cfg(feature = "program")]

use crate::{
    eth::{initialize, new_block},
    instruction::Instruction,
};

use byteorder::{ByteOrder, LittleEndian};
use solana_sdk::{
    account_info::{next_account_info, AccountInfo},
    entrypoint_deprecated::ProgramResult,
    info,
    program_error::ProgramError,
    pubkey::Pubkey,
};
use std::mem;

// Program entrypoint's implementation
pub fn process_hello<'a>(
    program_id: &Pubkey, // Public key of the account the hello world program was loaded into
    accounts: &'a [AccountInfo<'a>], // The account to say hello to
    _instruction_data: &[u8], // Ignored, all helloworld instructions are hellos
) -> ProgramResult {
    info!("Helloworld Rust program entrypoint");

    // Iterating accounts is safer then indexing
    let accounts_iter = &mut accounts.iter();

    // Get the account to say hello to
    let account = next_account_info(accounts_iter)?;

    // The account must be owned by the program in order to modify its data
    if account.owner != program_id {
        info!("Greeted account does not have the correct program id");
        return Err(ProgramError::IncorrectProgramId);
    }

    // The data must be large enough to hold a u64 count
    if account.try_data_len()? < mem::size_of::<u32>() {
        info!("Greeted account data length too small for u32");
        return Err(ProgramError::InvalidAccountData);
    }

    // Increment and store the number of times the account has been greeted
    let mut data = account.try_borrow_mut_data()?;
    let mut num_greets = LittleEndian::read_u32(&data);
    num_greets += 1;
    LittleEndian::write_u32(&mut data[0..], num_greets);

    info!("Hello!");

    Ok(())
}

pub fn process_instruction<'a>(
    program_id: &Pubkey,
    accounts: &'a [AccountInfo<'a>],
    instruction_data: &[u8],
) -> ProgramResult {
    info!("Ethereum light client entrypoint");

    let accounts_iter = &mut accounts.iter();
    let account = next_account_info(accounts_iter)?;

    let new_state = match Instruction::unpack(instruction_data)? {
        Instruction::Initialize(header) => initialize(header),
        Instruction::NewBlock(header) => new_block(account.deserialize_data().map_err(|_| ProgramError::InvalidAccountData)?, header),
    };

    account_serialize_data(account, &new_state)?;

    return Ok(());
}

fn account_serialize_data <T: serde::Serialize>(account: &AccountInfo, state: &T) -> Result<(), ProgramError> {
    let size = bincode::serialized_size(state).map_err(|_| ProgramError::InvalidAccountData)?;
    if size > account.data_len() as u64 {
        return Err(ProgramError::AccountDataTooSmall);
    }

    bincode::serialize_into(&mut account.data.borrow_mut()[..], state).map_err(|_| ProgramError::InvalidAccountData)?;
    return Ok(())
}
