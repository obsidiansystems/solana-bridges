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

fn process_new_block (mut account: AccountInfo, header_rlp: Rlp) -> Result<(), ProgramError> {
    //TODO: check account data size
    //TODO: initial state
    let mut state = account.deserialize_data().map_err(|_| ProgramError::InvalidAccountData)?;
    let hash = hash_header(&header_rlp);
    let header = BlockHeader::decode(&header_rlp).map_err(|_| ProgramError::InvalidInstructionData)?;

    if !verify(&state, &header) {
        return Err(ProgramError::InvalidInstructionData);
    }

    state.headers.insert(hash, header);
    account.serialize_data(&state).map_err(|_| ProgramError::Custom(1))?;
    return Ok(());
}

fn hash_header(header_rlp: &Rlp) -> H256 {
    let digest = Sha3_256::digest(header_rlp.as_raw());
    let hash = H256::from_slice(digest.as_slice());
    return hash;
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[derive(Debug)]
    enum TestError {
        HexError,
        RlpError,
    }

    fn hex_to_bytes(h: &str) -> Result<Vec<u8>, TestError> {
        return hex::decode(h).map_err(|_| TestError::HexError);
    }
    fn decode_header(bytes: &[u8]) -> Result<BlockHeader, TestError> {
        let rlp = Rlp::new(bytes);
        return BlockHeader::decode(&rlp).map_err(|_| TestError::RlpError);
    }
    fn encode_header(header: &BlockHeader) -> Vec<u8> {
        return header.rlp_bytes();
    }

    #[test]
    fn test_enc() -> Result<(), TestError>{
        let hex = "f9021aa0f779e50b45bc27e4ed236840e5dbcf7afab50beaf553be56bf76da977e10cc73a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479452bc44d5378309ee2abf1539bf71de1b7d7be3b5a014c996b6934d7991643669e145b8355c63aa02cbde63d390fcf4e6181d5eea45a079b7e79dc739c31662fe6f25f65bf5a5d14299c7a7aa42c3f75b9fb05474f54ca0e28dc05418692cb7baab7e7f85c1dedb8791c275b797ea3b1ffcaec5ef2aa271b9010000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000010000000000000000000000000000000000000000000000000000000408000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000010000000000000000000000000000000000000000000000000000000400000000000100000000000000000000000000080000000000000000000000000000000000000000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000903234373439353837313930323034343383890fe68395ba8e82d0d9845dd84a079150505945206e616e6f706f6f6c2e6f7267a0a35425f443452cf94ba4b698b00fd7b3ff4fc671dea3d5cc2dcbedbc3766f45e88af7fec6031063a17";
        let header = decode_header(&hex_to_bytes(hex)?)?;
        let expected = BlockHeader {
            parent_hash: H256::from([
                0xf7, 0x79, 0xe5, 0x0b, 0x45, 0xbc, 0x27, 0xe4,
                0xed, 0x23, 0x68, 0x40, 0xe5, 0xdb, 0xcf, 0x7a,
                0xfa, 0xb5, 0x0b, 0xea, 0xf5, 0x53, 0xbe, 0x56,
                0xbf, 0x76, 0xda, 0x97, 0x7e, 0x10, 0xcc, 0x73,
            ]),
            uncles_hash: H256::from([
                0x1d, 0xcc, 0x4d, 0xe8, 0xde, 0xc7, 0x5d, 0x7a,
                0xab, 0x85, 0xb5, 0x67, 0xb6, 0xcc, 0xd4, 0x1a,
                0xd3, 0x12, 0x45, 0x1b, 0x94, 0x8a, 0x74, 0x13,
                0xf0, 0xa1, 0x42, 0xfd, 0x40, 0xd4, 0x93, 0x47,
            ]),
            author: H160::from([
                0x52, 0xbc, 0x44, 0xd5,
                0x37, 0x83, 0x09, 0xee,
                0x2a, 0xbf, 0x15, 0x39,
                0xbf, 0x71, 0xde, 0x1b,
                0x7d, 0x7b, 0xe3, 0xb5,
            ]),
            state_root: H256::from([
                0x14, 0xc9, 0x96, 0xb6, 0x93, 0x4d, 0x79, 0x91,
                0x64, 0x36, 0x69, 0xe1, 0x45, 0xb8, 0x35, 0x5c,
                0x63, 0xaa, 0x02, 0xcb, 0xde, 0x63, 0xd3, 0x90,
                0xfc, 0xf4, 0xe6, 0x18, 0x1d, 0x5e, 0xea, 0x45,
            ]),
            transactions_root: H256::from([
                0x79, 0xb7, 0xe7, 0x9d, 0xc7, 0x39, 0xc3, 0x16,
                0x62, 0xfe, 0x6f, 0x25, 0xf6, 0x5b, 0xf5, 0xa5,
                0xd1, 0x42, 0x99, 0xc7, 0xa7, 0xaa, 0x42, 0xc3,
                0xf7, 0x5b, 0x9f, 0xb0, 0x54, 0x74, 0xf5, 0x4c,
            ]),
            receipts_root: H256::from([
                0xe2, 0x8d, 0xc0, 0x54, 0x18, 0x69, 0x2c, 0xb7,
                0xba, 0xab, 0x7e, 0x7f, 0x85, 0xc1, 0xde, 0xdb,
                0x87, 0x91, 0xc2, 0x75, 0xb7, 0x97, 0xea, 0x3b,
                0x1f, 0xfc, 0xae, 0xc5, 0xef, 0x2a, 0xa2, 0x71,
            ]),
            log_bloom: Bloom::from([
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x10, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ]),
            difficulty: U256::from_str("32343734393538373139303230343433").map_err(|_| TestError::HexError)?,
            number: 8982502,
            gas_limit: U256::from(9812622),
            gas_used: U256::from(53465),
            timestamp: 1574455815,
            extra_data: Vec::from([80, 80, 89, 69, 32, 110, 97, 110, 111, 112, 111, 111, 108, 46, 111, 114, 103]),
            mix_hash: H256::from([
                0xa3, 0x54, 0x25, 0xf4, 0x43, 0x45, 0x2c, 0xf9,
                0x4b, 0xa4, 0xb6, 0x98, 0xb0, 0x0f, 0xd7, 0xb3,
                0xff, 0x4f, 0xc6, 0x71, 0xde, 0xa3, 0xd5, 0xcc,
                0x2d, 0xcb, 0xed, 0xbc, 0x37, 0x66, 0xf4, 0x5e,
            ]),
            nonce: H64::from([
                0xaf, 0x7f, 0xec, 0x60, 0x31, 0x06, 0x3a, 0x17,
            ]),
        };

        assert_eq!(expected, decode_header(&encode_header(&expected))?);
        assert_eq!(header, expected);

        return Ok(());
    }
}
