#![allow(dead_code)]
use crate::parameters::*;

use fixed_hash::{construct_fixed_hash};
use uint::{construct_uint};
use impl_serde::{impl_fixed_hash_serde, impl_uint_serde};
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
use impl_rlp::{impl_fixed_hash_rlp, impl_uint_rlp};
use sha3::{Digest, Sha3_256};
use std::mem;
use arrayref::{array_mut_ref, array_ref, array_refs, mut_array_refs};

macro_rules! impls_uint {
    ($name: ident, $len: expr) => {
        impl_uint_serde!($name, $len);
        impl_uint_rlp!($name, $len);
        impl Sealed for $name {}

        //TODO: confirm we should assume provided buffer is correctly sized
        impl Pack for $name {
            const LEN: usize = $len * 8;
            fn pack_into_slice(&self, dst: &mut [u8]) {
                self.to_little_endian(dst);
            }
            fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
                return Ok($name::from_little_endian(src));
            }
        }
    };
}

macro_rules! impls_fixed_hash {
    ($name: ident, $len: expr) => {
        impl_fixed_hash_serde!($name, $len);
        impl_fixed_hash_rlp!($name, $len);
        impl Sealed for $name {}

        //TODO: confirm we should assume provided buffer is correctly sized
        impl Pack for $name {
            const LEN: usize = $len;
            fn pack_into_slice(&self, dst: &mut [u8]) {
                dst.copy_from_slice(self.as_bytes());
            }
            fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
                return Ok($name::from_slice(src));
            }
        }
    };
}


// Unsigned integers: bits(words)
construct_uint! { pub struct U128(2); }
construct_uint! { pub struct U256(4); }
construct_uint! { pub struct U512(8); }
impls_uint!(U128, 2);
impls_uint!(U256, 4);
impls_uint!(U512, 8);

// Hash types: bits(bytes)
construct_fixed_hash! { pub struct H64(8); }
construct_fixed_hash! { pub struct H128(16); }
construct_fixed_hash! { pub struct H160(20); }
construct_fixed_hash! { pub struct H256(32); }
construct_fixed_hash! { pub struct Bloom(256); }
impls_fixed_hash!(H64, 8);
impls_fixed_hash!(H128, 16);
impls_fixed_hash!(H160, 20);
impls_fixed_hash!(H256, 32);
impls_fixed_hash!(Bloom, 256);

pub type Address = H160;
pub type Height = u64;

#[derive(Debug, Eq, PartialEq)]
pub struct ExtraData {
    pub bytes: Vec<u8>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockHeader {
    pub parent_hash: H256,
    pub uncles_hash: H256,
    pub author: Address,
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

fn hash_header(header: &BlockHeader) -> H256 {
    return hash_rlp(&Rlp::new(&rlp::encode(header)));
}

fn hash_rlp(header_rlp: &Rlp) -> H256 {
    let digest = Sha3_256::digest(header_rlp.as_raw());
    let hash = H256::from_slice(digest.as_slice());
    return hash;
}

pub fn decode_header(header_rlp: &Rlp) -> Result<BlockHeader, ProgramError> {
    return BlockHeader::decode(header_rlp).map_err(|_| ProgramError::InvalidInstructionData);
}

pub fn initialize (header: BlockHeader) -> Result<State, ProgramError> {
    let mut initial = State {
        headers: HashMap::new(),
    };
    initial.headers.insert(hash_header(&header), header);
    return Ok(initial);
}

pub fn new_block (mut state: State, header: BlockHeader) -> Result<State, ProgramError> {
    if !verify(&state, &header) {
        return Err(ProgramError::InvalidInstructionData);
    }

    state.headers.insert(hash_header(&header), header);
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
            headers.insert(hash_header(&header), header);
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

pub const SIZE_OF_HEADER: usize =
      H256::LEN // parent_hash: H256
    + H256::LEN // uncles_hash: H256
    + Address::LEN // author: Address
    + H256::LEN // state_root: H256
    + H256::LEN // transactions_root: H256
    + H256::LEN // receipts_root: H256
    + Bloom::LEN // log_bloom: Bloom
    + U256::LEN // difficulty: U256
    + mem::size_of::<u64>() // number: u64
    + U256::LEN // gas_limit: U256
    + U256::LEN // gas_used: U256
    + mem::size_of::<u64>() // timestamp: u64
    + ExtraData::LEN // extra_data: ExtraData
    + H256::LEN // mix_hash: H256
    + H64::LEN //  nonce: H64
    ;

pub const HEADER_FIELD_SIZES: [usize; 15] = [
    H256::LEN, // parent_hash: H256
    H256::LEN, // uncles_hash: H256
    Address::LEN, // author: Address
    H256::LEN, // state_root: H256
    H256::LEN, // transactions_root: H256
    H256::LEN, // receipts_root: H256
    Bloom::LEN, // log_bloom: Bloom
    U256::LEN, // difficulty: U256
    mem::size_of::<u64>(), // number: u64
    U256::LEN, // gas_limit: U256
    U256::LEN, // gas_used: U256
    mem::size_of::<u64>(), // timestamp: u64
    ExtraData::LEN, // extra_data: ExtraData
    H256::LEN, // mix_hash: H256
    H64::LEN, //  nonce: H64
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
                            H256::LEN, // parent_hash: H256
                            H256::LEN, // uncles_hash: H256
                            Address::LEN, // author: Address
                            H256::LEN, // state_root: H256
                            H256::LEN, // transactions_root: H256
                            H256::LEN, // receipts_root: H256
                            Bloom::LEN, // log_bloom: Bloom
                            U256::LEN, // difficulty: U256
                            mem::size_of::<u64>(), // number: u64
                            U256::LEN, // gas_limit: U256
                            U256::LEN, // gas_used: U256
                            mem::size_of::<u64>(), // timestamp: u64
                            ExtraData::LEN, // extra_data: ExtraData
                            H256::LEN, // mix_hash: H256
                            H64::LEN //  nonce: H64
        ];

        self.parent_hash.pack_into_slice(parent_hash_dst);
        self.uncles_hash.pack_into_slice(uncles_hash_dst);
        self.author.pack_into_slice(author_dst);
        self.state_root.pack_into_slice(state_root_dst);
        self.transactions_root.pack_into_slice(transactions_root_dst);
        self.receipts_root.pack_into_slice(receipts_root_dst);
        self.log_bloom.pack_into_slice(log_bloom_dst);
        self.difficulty.pack_into_slice(difficulty_dst);
        *number_dst = self.number.to_le_bytes();
        self.gas_limit.pack_into_slice(gas_limit_dst);
        self.gas_used.pack_into_slice(gas_used_dst);
        *timestamp_dst = self.timestamp.to_le_bytes();
        self.extra_data.pack_into_slice(extra_data_dst);
        self.mix_hash.pack_into_slice(mix_hash_dst);
        self.nonce.pack_into_slice(nonce_dst);
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
                        H256::LEN, // parent_hash: H256
                        H256::LEN, // uncles_hash: H256
                        Address::LEN, // author: Address
                        H256::LEN, // state_root: H256
                        H256::LEN, // transactions_root: H256
                        H256::LEN, // receipts_root: H256
                        Bloom::LEN, // log_bloom: Bloom
                        U256::LEN, // difficulty: U256
                        mem::size_of::<u64>(), // number: u64
                        U256::LEN, // gas_limit: U256
                        U256::LEN, // gas_used: U256
                        mem::size_of::<u64>(), // timestamp: u64
                        ExtraData::LEN, // extra_data: ExtraData
                        H256::LEN, // mix_hash: H256
                        H64::LEN //  nonce: H64
        ];

        let header = BlockHeader {
            parent_hash: H256::unpack_from_slice(parent_hash_src)?,
            uncles_hash: H256::unpack_from_slice(uncles_hash_src)?,
            author: Address::unpack_from_slice(author_src)?,
            state_root: H256::unpack_from_slice(state_root_src)?,
            transactions_root: H256::unpack_from_slice(transactions_root_src)?,
            receipts_root: H256::unpack_from_slice(receipts_root_src)?,
            log_bloom: Bloom::unpack_from_slice(log_bloom_src)?,
            difficulty: U256::unpack_from_slice(difficulty_src)?,
            number: u64::from_le_bytes(*number_src),
            gas_limit: U256::unpack_from_slice(gas_limit_src)?,
            gas_used: U256::unpack_from_slice(gas_used_src)?,
            timestamp: u64::from_le_bytes(*timestamp_src),
            extra_data: ExtraData::unpack_from_slice(extra_data_src)?,
            mix_hash: H256::unpack_from_slice(mix_hash_src)?,
            nonce: H64::unpack_from_slice(nonce_src)?,
        };

        return Ok(header);
    }
}

impl Encodable for BlockHeader {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(HEADER_FIELD_SIZES.len());

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
        stream.append(&self.mix_hash);
        stream.append(&self.nonce);
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
