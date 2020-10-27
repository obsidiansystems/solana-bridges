//use rlp::DecodeError;

use solana_sdk::program_error::ProgramError;

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
    #[cfg(not(test))]
    DecodeChallengeInstructionFailed,

    #[cfg(test)]
    DecodeBlockFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeHeaderFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeDifficultyAndHeaderFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeInclusionInstructionFailed(rlp::DecoderError),
    #[cfg(test)]
    DecodeChallengeInstructionFailed(rlp::DecoderError),

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
    Challenge,
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
            Challenge => DecodeChallengeInstructionFailed,
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
            Challenge => DecodeChallengeInstructionFailed(e),
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
            DecodeDifficultyAndHeaderFailed(_) => 4,
            DecodeInclusionInstructionFailed(_) => 5,
            DecodeChallengeInstructionFailed(_) => 6,

            VerifyHeaderFailed_NonConsecutiveHeight => 7,
            VerifyHeaderFailed_NonMonotonicTimestamp => 8,
            VerifyHeaderFailed_InvalidParentHash => 9,
            VerifyHeaderFailed_TooMuchExtraData => 10,

            BlockNotFound => 11,
            UnpackExtraDataFailed => 12,
            InvalidAccountOwner => 13,
            DeserializeStorageFailed => 14,
            AlreadyInitialized => 15,
            WritableHistoryDuringProofCheck => 16,

            InvalidProof_BadBlockHash => 17,
            InvalidProof_TooEasy => 18,
            InvalidProof_BadMerkle => 19,
        })
    }
}
