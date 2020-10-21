//use rlp::DecodeError;

use solana_sdk::{
    program_error::ProgramError,
};

#[repr(u32)]
#[derive(Clone, Copy)]
pub enum CustomError {
    DecodeBlockFailed,
    DecodeHeaderFailed,
    VerifyHeaderFailed,
    BlockNotFound,
    UnpackExtraDataFailed,
    UnpackInstructionFailed,
    InvalidAccountOwner,
    DeserializeStorageFailed,
    AlreadyInitialized,
    InvalidProof,
    WritableHistoryDuringProofCheck,

    //DecodeError(DecodeError),
}

impl CustomError {
    pub fn to_program_error(&self) -> ProgramError {
        ProgramError::Custom(*self as u32)
    }
}
