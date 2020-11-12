use crate::{
    eth::*,
    types::*,
    pow_proof::AccessedElement,
};
use rlp::{self, Rlp};
use std::mem::size_of;

use ethereum_types::{H128, H256, H512, U256};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use solana_sdk::program_error::ProgramError;

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct Initialize {
    pub total_difficulty: Box<U256>,
    pub header: Box<BlockHeader>,
}

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct NewBlock {
    pub header: Box<BlockHeader>,
}

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct ProvidePowElement {
    pub element: Box<H512>,
}

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct ProveInclusion {
    pub height: u64,
    pub block_hash: Box<H256>,
    pub key: Vec<u8>,
    pub expected_value: Vec<u8>,
    pub proof: Vec<u8>,
    pub min_difficulty: Box<U256>,
}

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct Challenge {
    pub height: u64,
    pub block_hash: Box<H256>,
    pub merkle_spine: Vec<H128>,
}

// TODO don't reallocate for these, and instead lazily parse the instruction.
// That will get the instruction count down while continuing to keep the stack from growing too much
#[derive(Debug)]
pub enum Instruction {
    Noop,
    Initialize(Box<Initialize>),
    NewBlock(Box<NewBlock>),
    ProvidePowElement(Box<ProvidePowElement>),
    ProveInclusion(Box<ProveInclusion>),
    Challenge(Box<Challenge>),
}

impl Instruction {
    pub fn pack(&self) -> Vec<u8> {
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
            Self::ProvidePowElement(ref block) => {
                buf.push(3);
                buf.extend_from_slice(&rlp::encode(block));
            }
            Self::ProveInclusion(ref pi) => {
                buf.push(4);
                buf.extend_from_slice(&rlp::encode(pi));
            }
            Self::Challenge(ref c) => {
                buf.push(5);
                buf.extend_from_slice(&rlp::encode(c));
            }
        }
        return buf;
    }

    pub fn unpack(input: &[u8]) -> Result<Self, ProgramError> {
        let (&tag, rest) = input
            .split_first()
            .ok_or(CustomError::EmptyInstruction.to_program_error())?;
        let rlp = Rlp::new(rest);
        return match tag {
            0 => Ok(Self::Noop),
            1 => rlp
                .as_val()
                .map_err(|e| CustomError::from_rlp(DecodeFrom::DifficultyAndHeader, e))
                .map(Self::Initialize),
            2 => rlp
                .as_val()
                .map_err(|e| CustomError::from_rlp(DecodeFrom::Header, e))
                .map(Self::NewBlock),
            3 => rlp
                .as_val()
                .map_err(|e| CustomError::from_rlp(DecodeFrom::PowElement, e))
                .map(Self::ProvidePowElement),
            4 => rlp
                .as_val()
                .map_err(|e| CustomError::from_rlp(DecodeFrom::Inclusion, e))
                .map(Self::ProveInclusion),
            5 => rlp
                .as_val()
                .map_err(|e| CustomError::from_rlp(DecodeFrom::Challenge, e))
                .map(Self::Challenge),
            _ => Err(CustomError::InvalidInstructionTag),
        }
        .map_err(CustomError::to_program_error);
    }
}
