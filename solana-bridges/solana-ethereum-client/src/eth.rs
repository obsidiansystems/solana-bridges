#![allow(dead_code)]
use fixed_hash::{construct_fixed_hash};
use uint::{construct_uint};
use serde::{Deserialize, Serialize};
use impl_serde::{impl_fixed_hash_serde, impl_uint_serde};
use std::{
    collections::{HashMap},
    result::{Result},
    vec::{Vec},
};
use solana_sdk::{
    program_error::ProgramError,
    account_info::{AccountInfo},
};
use rlp::{
    Decodable, DecoderError, Encodable,
    Rlp, RlpStream,
};
use impl_rlp::{impl_fixed_hash_rlp, impl_uint_rlp};
use sha3::{Digest, Sha3_256};

macro_rules! impls_uint {
    ($name: ident, $len: expr) => {
        impl_uint_serde!($name, $len);
        impl_uint_rlp!($name, $len);
    };
}
macro_rules! impls_fixed_hash {
    ($name: ident, $len: expr) => {
        impl_fixed_hash_serde!($name, $len);
        impl_fixed_hash_rlp!($name, $len);
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
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
    pub extra_data: Vec<u8>,
    pub mix_hash: H256,
    pub nonce: H64,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct State {
    headers: HashMap<H256, BlockHeader>,
}

fn hash_header(header_rlp: &Rlp) -> H256 {
    let digest = Sha3_256::digest(header_rlp.as_raw());
    let hash = H256::from_slice(digest.as_slice());
    return hash;
}

fn parse_header(header_rlp: Rlp) -> Result<(H256, BlockHeader), ProgramError> {
    let hash = hash_header(&header_rlp);
    let header = BlockHeader::decode(&header_rlp).map_err(|_| ProgramError::InvalidInstructionData)?;
    return Ok((hash, header))
}

fn update_state(mut account: AccountInfo, state: &State) -> Result<(), ProgramError> {
    return account.serialize_data(state).map_err(|_| ProgramError::Custom(1));
}

fn bootstrap (account: AccountInfo, header_rlp: Rlp) -> Result<(), ProgramError> {
    let (hash, header) = parse_header(header_rlp)?;
    let mut initial = State {
        headers: HashMap::new(),
    };
    initial.headers.insert(hash, header);
    return update_state(account, &initial);
}

fn process_new_block (account: AccountInfo, header_rlp: Rlp) -> Result<(), ProgramError> {
    //TODO: check account data size
    let mut state = account.deserialize_data().map_err(|_| ProgramError::InvalidAccountData)?;
    let (hash, header) = parse_header(header_rlp)?;

    if !verify(&state, &header) {
        return Err(ProgramError::InvalidInstructionData);
    }

    state.headers.insert(hash, header);
    return update_state(account, &state);
}

fn verify(state: &State, header: &BlockHeader) -> bool {
    //TODO: genesis block?
    let parent = match state.headers.get(&header.parent_hash) {
        None => return false,
        Some(h) => h,
    };
    if header.number != (parent.number + 1) {
        return false;
    };

    return true;
}

impl Encodable for BlockHeader {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(15);

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
        stream.append(&self.extra_data);
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
            extra_data: serialized.val_at(12)?,
            mix_hash: serialized.val_at(13)?,
            nonce: serialized.val_at(14)?,
        };

        return Ok(block_header);
    }
}
