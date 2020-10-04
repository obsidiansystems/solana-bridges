use crate::eth::*;
use rlp::Rlp;
use std::mem::size_of;

use solana_sdk::program_error::ProgramError;

pub enum Instruction {
    Noop,
    Initialize(BlockHeader),
    NewBlock(BlockHeader),
}

impl Instruction {
    pub fn pack (&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(size_of::<Self>());

        match self {
            &Self::Noop => {
                buf.push(0);
            }
            &Self::Initialize(ref header) => {
                buf.push(1);
                buf.extend_from_slice(&rlp::encode(header));
            }
            &Self::NewBlock(ref header) => {
                buf.push(2);
                buf.extend_from_slice(&rlp::encode(header));
            }
        }
        return buf;
    }

    pub fn unpack(input: &[u8]) -> Result<Self, ProgramError> {
        let (&tag, rest) = input.split_first().ok_or(ProgramError::InvalidInstructionData)?;
        return match tag {
            0 => Ok(Self::Noop),
            1 => {
                let header = decode_header(&Rlp::new(rest))?;
                Ok(Self::Initialize(header))
            }
            2 => {
                let header = decode_header(&Rlp::new(rest))?;
                Ok(Self::NewBlock(header))
            }
            _ => return Err(ProgramError::InvalidInstructionData)
        };
    }

}
