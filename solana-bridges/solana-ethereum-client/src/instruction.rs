use crate::eth::*;
use rlp::Rlp;
use std::mem::size_of;

use solana_sdk::program_error::ProgramError;

pub enum Instruction {
    Initialize(BlockHeader),
    NewBlock(BlockHeader),
}

impl Instruction {
    pub fn pack (&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(size_of::<Self>());

        match self {
            &Self::Initialize(ref header) => {
                buf.push(0);
                buf.extend_from_slice(&rlp::encode(header));
            }
            &Self::NewBlock(ref header) => {
                buf.push(1);
                buf.extend_from_slice(&rlp::encode(header));
            }
        }
        return buf;
    }

    pub fn unpack(input: &[u8]) -> Result<Self, ProgramError> {
        let (&tag, rest) = input.split_first().ok_or(ProgramError::InvalidInstructionData)?;
        match tag {
            0 => {
                let header = decode_header(&Rlp::new(rest))?;
                return Ok(Self::Initialize(header));
            }
            1 => {
                let header = decode_header(&Rlp::new(rest))?;
                return Ok(Self::NewBlock(header));
            }
            _ => return Err(ProgramError::InvalidInstructionData)
        };
    }

}
