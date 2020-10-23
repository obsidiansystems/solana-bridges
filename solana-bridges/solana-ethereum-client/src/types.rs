//use rlp::DecodeError;

use solana_sdk::{
    program_error::ProgramError,
};

use rlp;

#[repr(u32)]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(not(test), derive(Copy))]
pub enum CustomError {
    EmptyInstruction,
    InvalidInstructionTag,

    #[cfg(not(test))]
    DecodeBlockFailed,
    #[cfg(not(test))]
    DecodeHeaderFailed,
    #[cfg(not(test))]
    DecodeDifficultyAndHeaderFailed,
    #[cfg(not(test))]
    DecodeInclusionInstructionFailed,

    #[cfg(test)]
    DecodeBlockFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeHeaderFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeDifficultyAndHeaderFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeInclusionInstructionFailed(rlp::DecoderError),

    #[allow(non_camel_case_types)]
    VerifyHeaderFailed_NonConsecutiveHeight,
    #[allow(non_camel_case_types)]
    VerifyHeaderFailed_NonMonotonicTimestamp,
    #[allow(non_camel_case_types)]
    VerifyHeaderFailed_InvalidParentHash,
    #[allow(non_camel_case_types)]
    VerifyHeaderFailed_TooMuchExtraData,

    BlockNotFound,
    UnpackExtraDataFailed,
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

pub enum DecodeFrom {
    Block,
    Header,
    DifficultyAndHeader,
    Inclusion,
}

impl CustomError {
    #[cfg(not(test))]
    pub fn from_rlp(t: DecodeFrom, _: rlp::DecoderError) -> Self {
        use CustomError::*;
        use DecodeFrom::*;
        match t {
            Block => DecodeBlockFailed,
            Header => DecodeHeaderFailed,
            DifficultyAndHeader => DecodeDifficultyAndHeaderFailed,
            Inclusion => DecodeInclusionInstructionFailed,
        }
    }
    #[cfg(test)]
    pub fn from_rlp(t: DecodeFrom, e: rlp::DecoderError) -> Self {
        use CustomError::*;
        use DecodeFrom::*;
        match t {
            Block => DecodeBlockFailed(e),
            Header => DecodeHeaderFailed(e),
            DifficultyAndHeader => DecodeDifficultyAndHeaderFailed(e),
            Inclusion => DecodeInclusionInstructionFailed(e),
        }
    }

    #[cfg(not(test))]
    pub fn to_program_error(self) -> ProgramError {
        ProgramError::Custom(self as u32)
    }
    #[cfg(test)]
    pub fn to_program_error(self) -> ProgramError {
        use CustomError::*;
        ProgramError::Custom(match self {
            EmptyInstruction => 0,
            InvalidInstructionTag => 1,

            DecodeBlockFailed(_) => 2,
            DecodeHeaderFailed(_) => 3,
            //DecodeDifficultyAndHeaderFailed(_) => 4,
            DecodeDifficultyAndHeaderFailed(e) => panic!("{}", e),
            DecodeInclusionInstructionFailed(_) => 5,

            VerifyHeaderFailed_NonConsecutiveHeight => 6,
            VerifyHeaderFailed_NonMonotonicTimestamp => 7,
            VerifyHeaderFailed_InvalidParentHash => 8,
            VerifyHeaderFailed_TooMuchExtraData => 9,

            BlockNotFound => 10,
            UnpackExtraDataFailed => 11,
            InvalidAccountOwner => 12,
            DeserializeStorageFailed => 13,
            AlreadyInitialized => 14,
            WritableHistoryDuringProofCheck => 15,

            InvalidProof_BadBlockHash => 16,
            InvalidProof_TooEasy => 17,
            InvalidProof_BadMerkle => 18,
        })
    }
}
