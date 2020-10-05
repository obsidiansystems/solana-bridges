use crate::eth::*;
use rlp::Rlp;
use std::mem::size_of;

use solana_sdk::program_error::ProgramError;

pub enum Instruction {
    Noop,
    Initialize(Block),
    NewBlock(Block),
}

impl Instruction {
    pub fn pack (&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(size_of::<Self>());

        match self {
            &Self::Noop => {
                buf.push(0);
            }
            &Self::Initialize(ref block) => {
                buf.push(1);
                buf.extend_from_slice(&rlp::encode(block));
            }
            &Self::NewBlock(ref block) => {
                buf.push(2);
                buf.extend_from_slice(&rlp::encode(block));
            }
        }
        return buf;
    }

    pub fn unpack(input: &[u8]) -> Result<Self, ProgramError> {
        let (&tag, rest) = input.split_first().ok_or(ProgramError::InvalidInstructionData)?;
        return match tag {
            0 => Ok(Self::Noop),
            1 => {
                let block = decode_block(&Rlp::new(rest))?;
                Ok(Self::Initialize(block))
            }
            2 => {
                let block = decode_block(&Rlp::new(rest))?;
                Ok(Self::NewBlock(block))
            }
            _ => return Err(ProgramError::InvalidInstructionData)
        };
    }

}
