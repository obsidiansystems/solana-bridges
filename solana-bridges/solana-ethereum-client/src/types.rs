use solana_sdk::{
    program_error::ProgramError,
};

pub enum CustomError {
    DecodeBlockFailed,
    DecodeHeaderFailed,
    VerifyHeaderFailed,
    NoParentBlock,
    UnpackExtraDataFailed,
    UnpackInstructionFailed,
    InvalidAccountOwner,
    DeserializeStorageFailed,
}

impl CustomError {
    pub fn to_program_error(&self) -> ProgramError {
        let code = match self {
            &Self::DecodeBlockFailed => 0,
            &Self::DecodeHeaderFailed => 1,
            &Self::VerifyHeaderFailed => 2,
            &Self::NoParentBlock => 3,
            &Self::UnpackExtraDataFailed => 4,
            &Self::UnpackInstructionFailed => 5,
            &Self::InvalidAccountOwner => 6,
            &Self::DeserializeStorageFailed => 7,
        };
        return ProgramError::Custom(code)
    }
}
