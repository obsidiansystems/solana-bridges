#![allow(dead_code)]
use crate::parameters::*;

use ethereum_types::{U256, H64, H160, H256, Bloom};
use std::{
    collections::{HashMap},
    result::{Result},
    vec::{Vec},
};
use solana_sdk::{
    program_error::ProgramError,
    program_pack::{Pack, Sealed},
};
use rlp::{
    Decodable, DecoderError, Encodable,
    Rlp, RlpStream,
};
use std::mem;
use arrayref::{array_mut_ref, array_ref, array_refs, mut_array_refs};

#[derive(Debug, Eq, PartialEq)]
pub struct ExtraData {
    pub bytes: Vec<u8>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockHeader {
    pub parent_hash: H256,
    pub uncles_hash: H256,
    pub author: H160,
    pub state_root: H256,
    pub transactions_root: H256,
    pub receipts_root: H256,
    pub log_bloom: Bloom,
    pub difficulty: U256,
    pub number: u64,
    pub gas_limit: U256,
    pub gas_used: U256,
    pub timestamp: u64,
    pub extra_data: ExtraData,
    pub mix_hash: H256,
    pub nonce: H64,
}

#[derive(Debug)]
pub struct State {
    pub headers: HashMap<H256, BlockHeader>,
}

fn hash_header(header: &BlockHeader, truncated: bool) -> H256 {
    let mut stream = RlpStream::new();
    header.stream_rlp(&mut stream, truncated);
    return keccak256(stream.out().as_slice());
}

fn keccak256(bytes: &[u8]) -> H256 {
    use sha3::{Digest, Keccak256};
    let digest = Keccak256::digest(bytes);
    return H256::from_slice(digest.as_slice());
}

pub fn decode_header(header_rlp: &Rlp) -> Result<BlockHeader, ProgramError> {
    return BlockHeader::decode(header_rlp).map_err(|_| ProgramError::InvalidInstructionData);
}

pub fn initialize (header: BlockHeader) -> Result<State, ProgramError> {
    let mut initial = State {
        headers: HashMap::new(),
    };
    initial.headers.insert(hash_header(&header, false), header);
    return Ok(initial);
}

pub fn new_block (mut state: State, header: BlockHeader) -> Result<State, ProgramError> {
    if !verify(&state, &header) {
        return Err(ProgramError::InvalidInstructionData);
    }

    state.headers.insert(hash_header(&header, false), header);
    return Ok(state);
}

pub fn verify(state: &State, header: &BlockHeader) -> bool {
    let parent = match state.headers.get(&header.parent_hash) {
        None => return false,
        Some(h) => h,
    };
    if header.number != (parent.number + 1) {
        return false;
    };

    return true;
}

impl Sealed for State {}
impl Pack for State {
    const LEN: usize = 1 + BlockHeader::LEN * HEADER_HISTORY_SIZE;
    fn pack_into_slice(&self, dst: &mut [u8]) {
        const LENGTH_SIZE: usize = mem::size_of::<usize>();
        let length_dst = array_mut_ref![dst, 0, LENGTH_SIZE];
        *length_dst = self.headers.len().to_le_bytes();

        for (i, h) in self.headers.values().enumerate() {
            let dst_array = array_mut_ref![dst, LENGTH_SIZE + BlockHeader::LEN * i, BlockHeader::LEN];
            h.pack_into_slice(dst_array);
        }
    }

    fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
        const LENGTH_SIZE: usize = mem::size_of::<usize>();
        let length_src = array_ref![src, 0, LENGTH_SIZE];
        let length = usize::from_le_bytes(*length_src);

        let mut headers: HashMap<H256,BlockHeader> = HashMap::new();
        for i in 0..length {
            let header_src = array_ref![src, LENGTH_SIZE + BlockHeader::LEN * i, BlockHeader::LEN];
            let header = BlockHeader::unpack_from_slice(header_src)?;
            headers.insert(hash_header(&header, false), header);
        }
        return Ok(State { headers })
    }
}

impl Sealed for ExtraData {}
impl Pack for ExtraData {
    const LEN: usize = 1 + 32;
    fn pack_into_slice(&self, dst: &mut [u8]) {
        let len = self.bytes.len();
        dst[0] = len as u8;
        dst[1..len+1].copy_from_slice(&self.bytes);
    }
    fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
        let (&size, rest) = src.split_first().ok_or(ProgramError::InvalidInstructionData)?;
        return Ok(ExtraData { bytes: rest[0..size as usize].to_vec() });
    }
}

const H64_LEN: usize = 8;
const H160_LEN: usize = 20;
const H256_LEN: usize = 32;
const U256_LEN: usize = 32;
const BLOOM_LEN: usize = 256;
pub const SIZE_OF_HEADER: usize =
      H256_LEN // parent_hash: H256
    + H256_LEN // uncles_hash: H256
    + H160_LEN // author: H160
    + H256_LEN // state_root: H256
    + H256_LEN // transactions_root: H256
    + H256_LEN // receipts_root: H256
    + BLOOM_LEN // log_bloom: Bloom
    + U256_LEN // difficulty: U256
    + mem::size_of::<u64>() // number: u64
    + U256_LEN // gas_limit: U256
    + U256_LEN // gas_used: U256
    + mem::size_of::<u64>() // timestamp: u64
    + ExtraData::LEN // extra_data: ExtraData
    + H256_LEN // mix_hash: H256
    + H64_LEN //  nonce: H64
    ;

pub const HEADER_FIELD_SIZES: [usize; 15] = [
    H256_LEN, // parent_hash: H256
    H256_LEN, // uncles_hash: H256
    H160_LEN, // author: H160
    H256_LEN, // state_root: H256
    H256_LEN, // transactions_root: H256
    H256_LEN, // receipts_root: H256
    BLOOM_LEN, // log_bloom: Bloom
    U256_LEN, // difficulty: U256
    mem::size_of::<u64>(), // number: u64
    U256_LEN, // gas_limit: U256
    U256_LEN, // gas_used: U256
    mem::size_of::<u64>(), // timestamp: u64
    ExtraData::LEN, // extra_data: ExtraData
    H256_LEN, // mix_hash: H256
    H64_LEN, //  nonce: H64
];

impl Sealed for BlockHeader {}
impl Pack for BlockHeader {
    const LEN: usize = SIZE_OF_HEADER;
    fn pack_into_slice(&self, dst: &mut [u8]) {
        let dst = array_mut_ref![dst, 0, BlockHeader::LEN];
        let (
            parent_hash_dst,
            uncles_hash_dst,
            author_dst,
            state_root_dst,
            transactions_root_dst,
            receipts_root_dst,
            log_bloom_dst,
            difficulty_dst,
            number_dst,
            gas_limit_dst,
            gas_used_dst,
            timestamp_dst,
            extra_data_dst,
            mix_hash_dst,
            nonce_dst,
        ) = mut_array_refs![dst,
                            H256_LEN, // parent_hash: H256
                            H256_LEN, // uncles_hash: H256
                            H160_LEN, // author: H160
                            H256_LEN, // state_root: H256
                            H256_LEN, // transactions_root: H256
                            H256_LEN, // receipts_root: H256
                            BLOOM_LEN, // log_bloom: Bloom
                            U256_LEN, // difficulty: U256
                            mem::size_of::<u64>(), // number: u64
                            U256_LEN, // gas_limit: U256
                            U256_LEN, // gas_used: U256
                            mem::size_of::<u64>(), // timestamp: u64
                            ExtraData::LEN, // extra_data: ExtraData
                            H256_LEN, // mix_hash: H256
                            H64_LEN //  nonce: H64
        ];

        *parent_hash_dst = *self.parent_hash.as_fixed_bytes();
        *uncles_hash_dst = *self.uncles_hash.as_fixed_bytes();
        *author_dst = *self.author.as_fixed_bytes();
        *state_root_dst = *self.state_root.as_fixed_bytes();
        *transactions_root_dst = *self.transactions_root.as_fixed_bytes();
        *receipts_root_dst = *self.receipts_root.as_fixed_bytes();
        *log_bloom_dst = *self.log_bloom.as_fixed_bytes();
        self.difficulty.to_little_endian(difficulty_dst);
        *number_dst = self.number.to_le_bytes();
        self.gas_limit.to_little_endian(gas_limit_dst);
        self.gas_used.to_little_endian(gas_used_dst);
        *timestamp_dst = self.timestamp.to_le_bytes();
        self.extra_data.pack_into_slice(extra_data_dst);
        *mix_hash_dst = *self.mix_hash.as_fixed_bytes();
        *nonce_dst = *self.nonce.as_fixed_bytes();
    }

    fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
        let src = array_ref![src, 0, BlockHeader::LEN];
        let (
            parent_hash_src,
            uncles_hash_src,
            author_src,
            state_root_src,
            transactions_root_src,
            receipts_root_src,
            log_bloom_src,
            difficulty_src,
            number_src,
            gas_limit_src,
            gas_used_src,
            timestamp_src,
            extra_data_src,
            mix_hash_src,
            nonce_src,
        ) = array_refs![src,
                        H256_LEN, // parent_hash: H256
                        H256_LEN, // uncles_hash: H256
                        H160_LEN, // author: H160
                        H256_LEN, // state_root: H256
                        H256_LEN, // transactions_root: H256
                        H256_LEN, // receipts_root: H256
                        BLOOM_LEN, // log_bloom: Bloom
                        U256_LEN, // difficulty: U256
                        mem::size_of::<u64>(), // number: u64
                        U256_LEN, // gas_limit: U256
                        U256_LEN, // gas_used: U256
                        mem::size_of::<u64>(), // timestamp: u64
                        ExtraData::LEN, // extra_data: ExtraData
                        H256_LEN, // mix_hash: H256
                        H64_LEN //  nonce: H64
        ];

        let header = BlockHeader {
            parent_hash: H256::from(parent_hash_src),
            uncles_hash: H256::from(uncles_hash_src),
            author: H160::from(author_src),
            state_root: H256::from(state_root_src),
            transactions_root: H256::from(transactions_root_src),
            receipts_root: H256::from(receipts_root_src),
            log_bloom: Bloom::from(log_bloom_src),
            difficulty: U256::from_little_endian(difficulty_src),
            number: u64::from_le_bytes(*number_src),
            gas_limit: U256::from_little_endian(gas_limit_src),
            gas_used: U256::from_little_endian(gas_used_src),
            timestamp: u64::from_le_bytes(*timestamp_src),
            extra_data: ExtraData::unpack_from_slice(extra_data_src)?,
            mix_hash: H256::from(mix_hash_src),
            nonce: H64::from(nonce_src),
        };

        return Ok(header);
    }
}

impl BlockHeader {
    fn stream_rlp(&self, stream: &mut RlpStream, truncated: bool) {
        stream.begin_list(HEADER_FIELD_SIZES.len() - if truncated { 2 } else { 0 });

        stream.append(&self.parent_hash);
        stream.append(&self.uncles_hash);
        stream.append(&self.author);
        stream.append(&self.state_root);
        stream.append(&self.transactions_root);
        stream.append(&self.receipts_root);
        stream.append(&self.log_bloom);
        stream.append(&self.difficulty);
        stream.append(&self.number);
        stream.append(&self.gas_limit);
        stream.append(&self.gas_used);
        stream.append(&self.timestamp);
        stream.append(&self.extra_data.bytes);

        if !truncated {
            stream.append(&self.mix_hash);
            stream.append(&self.nonce);
        }
    }
}

impl Encodable for BlockHeader {
    fn rlp_append(&self, stream: &mut RlpStream) {
        self.stream_rlp(stream, false);
    }
}

impl Decodable for BlockHeader {
    fn decode(serialized: &Rlp) -> Result<Self, DecoderError> {
        let block_header = BlockHeader {
            parent_hash: serialized.val_at(0)?,
            uncles_hash: serialized.val_at(1)?,
            author: serialized.val_at(2)?,
            state_root: serialized.val_at(3)?,
            transactions_root: serialized.val_at(4)?,
            receipts_root: serialized.val_at(5)?,
            log_bloom: serialized.val_at(6)?,
            difficulty: serialized.val_at(7)?,
            number: serialized.val_at(8)?,
            gas_limit: serialized.val_at(9)?,
            gas_used: serialized.val_at(10)?,
            timestamp: serialized.val_at(11)?,
            extra_data: ExtraData { bytes: serialized.val_at(12)? },
            mix_hash: serialized.val_at(13)?,
            nonce: serialized.val_at(14)?,
        };

        return Ok(block_header);
    }
}
