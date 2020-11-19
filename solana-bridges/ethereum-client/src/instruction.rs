use crate::{
    eth::*,
    types::*,
    pow_proof::*,
};
use arrayref::array_ref;

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


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ProvidePowElement {
    /// Height of block for which the elements are used
    pub height: u64,
    /// offset of a chunk of 8 contiguous elements, in [0, 16) since 16 * 8 is 128.
    pub chunk_offset: u8,
    pub elements: [H512; Self::ETHASH_ELEMENTS_PER_INSTRUCTION as usize],
}

impl ProvidePowElement {
    pub const ETHASH_ELEMENTS_PER_INSTRUCTION: u8 = 8;

    pub fn new (height: u64, chunk_offset: u8) -> Self {
        Self {
            height,
            chunk_offset,
            elements: [H512::zero(); Self::ETHASH_ELEMENTS_PER_INSTRUCTION as usize],
        }
    }
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
    /// in access order
    pub element_index: u8,
    pub merkle_spine: Vec<H128>,
    pub element_pair: Box<ElementPair>,
}

// TODO don't reallocate for these, and instead lazily parse the instruction.
// That will get the instruction count down while continuing to keep the stack from growing too much
#[derive(Debug)]
pub enum Instruction {
    Noop,
    Initialize(Box<Initialize>),
    NewBlock(Box<BlockHeader>),
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
                buf.extend_from_slice(&block.height.to_le_bytes());
                buf.push(block.chunk_offset);
                for e in &block.elements {
                    buf.extend_from_slice(&e.to_fixed_bytes());
                }
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
        let mut rest = Parser(input);
        let tag = rest.pop()?;
        let rlp = Rlp::new(rest.peek());
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
            3 => {
                let height_bytes = rest.pop_many(8)?;
                let chunk_offset = rest.pop()?;
                let mut ppe = ProvidePowElement::new(
                    u64::from_le_bytes(*array_ref!(height_bytes, 0, 8)),
                    chunk_offset
                );
                for i in 0..ProvidePowElement::ETHASH_ELEMENTS_PER_INSTRUCTION {
                    let bytes: &[u8; 64] = array_ref!(rest.peek(), 64 * (i as usize), 64);
                    ppe.elements[i as usize] = H512::from_slice(bytes);
                }
                Ok(Self::ProvidePowElement(Box::new(ppe)))
            },
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

#[derive(Clone, Copy)]
struct Parser<'a>(&'a [u8]);

impl<'a> Parser<'a> {
    fn pop(&mut self) -> Result<u8, ProgramError> {
        let (&v, new) = self.0
            .split_first()
            .ok_or(CustomError::IncompleteInstruction.to_program_error())?;
        self.0 = new;
        Ok(v)
    }
    fn pop_many(&mut self, n: usize) -> Result<&'a [u8], ProgramError> {
        if n > self.0.len() {
            return Err(CustomError::IncompleteInstruction.to_program_error());
        }
        let (v, new) = self.0
            .split_at(n);
        self.0 = new;
        Ok(v)
    }
    fn peek(self) -> &'a [u8] {
        self.0
    }
}
