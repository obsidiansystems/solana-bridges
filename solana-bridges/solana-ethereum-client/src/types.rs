//use rlp::DecodeError;

use solana_sdk::{
    program_error::ProgramError,
};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CustomError {
    DecodeBlockFailed,
    DecodeHeaderFailed,
    DecodeDifficultyAndHeaderFailed,
    VerifyHeaderFailed,
    BlockNotFound,
    UnpackExtraDataFailed,
    UnpackInstructionFailed,
    InvalidAccountOwner,
    DeserializeStorageFailed,
    AlreadyInitialized,
    WritableHistoryDuringProofCheck,

    #[allow(non_camel_case_types)]
    InvalidProof_BadBlockHash,
    #[allow(non_camel_case_types)]
    InvalidProof_TooEasy,
    #[allow(non_camel_case_types)]
    InvalidProof_BadMerkle,
}

impl CustomError {
    pub fn to_program_error(&self) -> ProgramError {
        ProgramError::Custom(*self as u32)
    }
}
