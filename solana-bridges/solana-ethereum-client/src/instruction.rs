use crate::eth::*;
use crate::types::*;
use crate::parameters::*;
use rlp::{self, Rlp};
use std::mem::size_of;

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use solana_sdk::program_error::ProgramError;

// TODO don't reallocate for this, and instead don't eagerly parse the instruction.
#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct ProveInclusion {
    pub height: u64,
    pub block_hash: ethereum_types::H256,
    pub key: Vec<u8>,
    pub expected_value: Vec<u8>,
    pub proof: Vec<u8>,
}

pub enum Instruction {
    Noop,
    Initialize(RingItem),
    NewBlock(BlockHeader),
    ProveInclusion(ProveInclusion),
}

impl Instruction {
    pub fn pack (&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(size_of::<Self>());

        match *self {
            Self::Noop => {
                buf.push(0);
            }
            Self::Initialize(ref block) => {
                buf.push(1);
                buf.extend_from_slice(&rlp::encode(block));
            }
            Self::NewBlock(ref block) => {
                buf.push(2);
                buf.extend_from_slice(&rlp::encode(block));
            }
            Self::ProveInclusion(ref pi) => {
                buf.push(3);
                buf.extend_from_slice(&rlp::encode(pi));
            }
        }
        return buf;
    }

    pub fn unpack(input: &[u8]) -> Result<Self, ProgramError> {
        let (&tag, rest) = input.split_first().ok_or(CustomError::UnpackInstructionFailed.to_program_error())?;
        return match tag {
            0 => Ok(Self::Noop),
            1 => {
                let block = rlp::decode(rest)
                    .map_err(|_| CustomError::DecodeHeaderFailed.to_program_error())?;
                Ok(Self::Initialize(block))
            }
            2 => {
                let block = decode_header(&Rlp::new(rest))?;
                Ok(Self::NewBlock(block))
            }
            3 => {
                Rlp::new(rest)
                    .as_val()
                    .map_err(|_| CustomError::UnpackInstructionFailed.to_program_error())
                    .map(Self::ProveInclusion)
            }
            _ => return Err(CustomError::UnpackInstructionFailed.to_program_error())
        };
    }

}
