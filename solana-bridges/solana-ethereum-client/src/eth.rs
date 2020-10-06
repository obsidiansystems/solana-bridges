use crate::parameters::*;
use crate::types::*;

use ethereum_types::{U256, H64, H160, H256, Bloom};
use std::{
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
use tiny_keccak::{Hasher, Keccak};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExtraData {
    pub bytes: Vec<u8>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

//TODO: determine maximum widths to support per field
type Scalar = U256;

pub struct TransactionData {
    pub bytes: Vec<u8>,
}

pub enum TransactionAction {
    Call(H160), //TODO: transfer?
    Create,
}

impl Encodable for TransactionAction {
    fn rlp_append(&self, stream: &mut RlpStream) {
        match self {
            &TransactionAction::Call(address) => stream.append(&address),
            &TransactionAction::Create => stream.begin_list(0),
        };
    }
}

impl Decodable for TransactionAction {
    fn decode(rlp: &Rlp) -> Result<Self, DecoderError> {
    Ok(if rlp.is_empty() {
            TransactionAction::Create
        } else {
            TransactionAction::Call(rlp.as_val()?)
        })
    }
}

pub struct Transaction {
    pub nonce: Scalar,
    pub gas_price: Scalar,
    pub gas_limit: Scalar,
    pub to: TransactionAction,
    pub value: Scalar,
    pub data: TransactionData,
    pub v: U256,
    pub r: U256,
    pub s: U256,
}

impl Encodable for Transaction {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(9);
        stream.append(&self.nonce);
        stream.append(&self.gas_price);
        stream.append(&self.gas_limit);
        stream.append(&self.to);
        stream.append(&self.value);
        stream.append(&self.data.bytes);
        stream.append(&self.v);
        stream.append(&self.r);
        stream.append(&self.s);
    }
}

impl Decodable for Transaction {
    fn decode(serialized: &Rlp) -> Result<Self, DecoderError> {
        let res = Transaction {
            nonce: serialized.val_at(0)?,
            gas_price: serialized.val_at(1)?,
            gas_limit: serialized.val_at(2)?,
            to: serialized.val_at(3)?,
            value: serialized.val_at(4)?,
            data: TransactionData { bytes: serialized.val_at(5)? },
            v: serialized.val_at(6)?,
            r: serialized.val_at(7)?,
            s: serialized.val_at(8)?,
        };
        return Ok(res);
    }
}

pub struct Block {
    pub header: BlockHeader,
    pub transactions: Vec<Transaction>,
}

impl Decodable for Block {
    fn decode(serialized: &Rlp) -> Result<Self, DecoderError> {
        let res = Block {
            header: serialized.val_at(0)?,
            transactions: serialized.list_at(1)?,
        };
        return Ok(res);
    }
}

impl Encodable for Block {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(2);
        stream.append(&self.header);
        stream.append_list(&self.transactions);
    }
}

#[derive(Debug)]
pub struct State {
    pub headers: Vec<BlockHeader>,
}

pub fn hash_header(header: &BlockHeader, truncated: bool) -> H256 {
    let mut stream = RlpStream::new();
    header.stream_rlp(&mut stream, truncated);
    return keccak256(stream.out().as_slice());
}

fn keccak256(bytes: &[u8]) -> H256 {
    let mut keccak256 = Keccak::v256();
    let mut out = [0u8; 32];
    keccak256.update(bytes);
    keccak256.finalize(&mut out);
    H256::from(out)
}

pub fn decode_block(block_rlp: &Rlp) -> Result<Block, ProgramError> {
    return Block::decode(block_rlp).map_err(|_| CustomError::DecodeBlockFailed.to_program_error());
}

pub fn decode_header(header_rlp: &Rlp) -> Result<BlockHeader, ProgramError> {
    return BlockHeader::decode(header_rlp).map_err(|_| CustomError::DecodeHeaderFailed.to_program_error());
}

pub fn initialize (header: BlockHeader) -> Result<State, ProgramError> {
    if !verify_block(&header, None) {
        return Err(CustomError::VerifyHeaderFailed.to_program_error());
    };

    let mut initial = State {
        headers: Vec::new(),
    };

    initial.headers.push(header);
    return Ok(initial);
}

pub fn new_block (mut state: State, header: BlockHeader) -> Result<State, ProgramError> {
    let parent = match state.headers.get(state.headers.len() - 1) {
        None => return Err(CustomError::NoParentBlock.to_program_error()),
        Some(h) => h,
    };

    if !verify_block(&header, Some(parent)) {
        return Err(CustomError::VerifyHeaderFailed.to_program_error());
    };

    state.headers.push(header);
    return Ok(state);
}

pub fn verify_block(header: &BlockHeader, parent: Option<&BlockHeader>) -> bool {
    let parent_check = match parent {
        None => true,
        Some(p) =>
            header.number == p.number + 1
            && header.timestamp > p.timestamp
            && header.parent_hash == hash_header(p, false)
    };

    let self_check =
        header.extra_data.bytes.len() <= 32
        && verify_pow(header);

    return self_check && parent_check;
}

pub fn verify_pow(header: &BlockHeader) -> bool {
    use ethash::*;
    const EPOCH_LENGTH: u64 = 30000;
    let epoch = (header.number / EPOCH_LENGTH) as usize;
    let seed = get_seedhash(epoch);
    let cache_size = get_cache_size(epoch);
    let full_size = get_full_size(epoch);

    let mut cache = vec![0; cache_size];
    make_cache(&mut cache, seed);

    let (_mix_hash, result) = hashimoto_light(hash_header(&header, true), header.nonce, full_size, &cache);
    let target = cross_boundary(header.difficulty);

    return U256::from_big_endian(result.as_fixed_bytes()) <= target;
}

impl Sealed for State {}
impl Pack for State {
    const LEN: usize = 1 + BlockHeader::LEN * HEADER_HISTORY_SIZE;
    fn pack_into_slice(&self, dst: &mut [u8]) {
        const LENGTH_SIZE: usize = mem::size_of::<usize>();
        let length_dst = array_mut_ref![dst, 0, LENGTH_SIZE];
        *length_dst = self.headers.len().to_le_bytes();

        for (i, h) in self.headers.iter().enumerate() {
            let dst_array = array_mut_ref![dst, LENGTH_SIZE + BlockHeader::LEN * i, BlockHeader::LEN];
            h.pack_into_slice(dst_array);
        }
    }

    fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
        const LENGTH_SIZE: usize = mem::size_of::<usize>();
        let length_src = array_ref![src, 0, LENGTH_SIZE];
        let length = usize::from_le_bytes(*length_src);

        let mut headers: Vec<BlockHeader> = Vec::new();
        for i in 0..length {
            let header_src = array_ref![src, LENGTH_SIZE + BlockHeader::LEN * i, BlockHeader::LEN];
            let header = BlockHeader::unpack_from_slice(header_src)?;
            headers.push(header);
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
        let (&size, rest) = src.split_first().ok_or(CustomError::DecodeHeaderFailed.to_program_error())?;
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
