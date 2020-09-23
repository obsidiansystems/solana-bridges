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
use bincode::{serialize, deserialize};
use solana_sdk::{program_error::ProgramError};


// Unsigned integers: bits(words)
construct_uint! { pub struct U128(2); }
construct_uint! { pub struct U256(4); }
construct_uint! { pub struct U512(8); }

// Hash types: bits(bytes)
construct_fixed_hash! { pub struct H64(8); }
construct_fixed_hash! { pub struct H128(16); }
construct_fixed_hash! { pub struct H160(20); }
construct_fixed_hash! { pub struct H256(32); }
construct_fixed_hash! { pub struct H512(64); }
construct_fixed_hash! { pub struct Bloom(256); }

impl_uint_serde!(U128, 2);
impl_uint_serde!(U256, 4);
impl_uint_serde!(U512, 8);

impl_fixed_hash_serde!(H64, 8);
impl_fixed_hash_serde!(H128, 16);
impl_fixed_hash_serde!(H160, 20);
impl_fixed_hash_serde!(H256, 32);
impl_fixed_hash_serde!(H512, 64);
impl_fixed_hash_serde!(Bloom, 256);

pub type Address = H160;
pub type Height = u64;

#[derive(Serialize, Deserialize, Debug)]
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

    pub hash: Option<H256>,
    pub partial_hash: Option<H256>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct State {
    header_hashes: HashMap<Height, Vec<H256>>,
    headers: HashMap<H256, BlockHeader>,
}

pub fn pack(state: State) -> Result<Vec<u8>, ProgramError> {
    return serialize(&state).map_err(|_| ProgramError::InvalidAccountData);
}
pub fn unpack(bytes: &[u8]) -> Result<State, ProgramError> {
    return deserialize(bytes).map_err(|_| ProgramError::InvalidAccountData);
}
