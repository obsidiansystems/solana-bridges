use ethereum_types::{Bloom, H160, H256, H512, H64, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};
use std::{result::Result, vec::Vec};

use tiny_keccak::{Hasher, Keccak};

use crate::types::*;

pub const EXTRA_DATA_MAX_LEN: usize = 32;

pub const EPOCH_LENGTH: u64 = 30000;

#[derive(Debug, Clone, Copy)]
pub struct ExtraData {
    len: u8,
    bytes: [u8; EXTRA_DATA_MAX_LEN],
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

#[derive(Debug, Eq, PartialEq, Clone, RlpEncodableDerive, RlpDecodableDerive)]
pub struct Receipt {
    pub status: bool,
    pub gas_used: U256,
    pub log_bloom: Bloom,
    pub logs: Vec<LogEntry>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LogEntry {
    pub address: H160,
    pub topics: Vec<H256>,
    pub data: Vec<u8>,
}

impl rlp::Decodable for LogEntry {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let result = LogEntry {
            address: rlp.val_at(0)?,
            topics: rlp.list_at(1)?,
            data: rlp.val_at(2)?,
        };
        Ok(result)
    }
}

impl rlp::Encodable for LogEntry {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(3usize);
        stream.append(&self.address);
        stream.append_list::<H256, _>(&self.topics);
        stream.append(&self.data);
    }
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
            data: TransactionData {
                bytes: serialized.val_at(5)?,
            },
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

pub fn hash_header(header: &BlockHeader, truncated: bool) -> H256 {
    let mut stream = RlpStream::new();
    header.stream_rlp(&mut stream, truncated);
    return keccak256(stream.out().as_slice());
}

pub fn keccak256(bytes: &[u8]) -> H256 {
    let mut keccak256 = Keccak::v256();
    let mut out = [0u8; 32];
    keccak256.update(bytes);
    keccak256.finalize(&mut out);
    H256::from(out)
}

pub fn verify_block(header: &BlockHeader, parent: Option<&BlockHeader>) -> Result<(), CustomError> {
    use CustomError::*;

    if let Some(p) = parent {
        if header.number != p.number + 1 {
            return Err(VerifyHeaderFailed_NonConsecutiveHeight);
        }
        if header.timestamp <= p.timestamp {
            return Err(VerifyHeaderFailed_NonMonotonicTimestamp);
        }
        if header.parent_hash != hash_header(p, false) {
            return Err(VerifyHeaderFailed_InvalidParentHash);
        }
    };

    if header.extra_data.bytes.len() > 32 {
        return Err(VerifyHeaderFailed_TooMuchExtraData);
    }

    Ok(())
}

pub fn height_to_epoch(h: u64) -> u64 {
    h / EPOCH_LENGTH
}

pub fn verify_pow<F>(header: &BlockHeader, lookup: F) -> bool
where
    F: FnMut(u32) -> H512,
{
    use ethash::*;
    let epoch = height_to_epoch(header.number) as usize;
    let full_size = get_full_size(epoch);

    let (_mix_hash, result) =
        hashimoto(hash_header(&header, true), header.nonce, full_size, lookup);
    let target = cross_boundary(header.difficulty);

    return U256::from_big_endian(result.as_fixed_bytes()) <= target;
}

impl BlockHeader {
    const NUM_FIELDS: usize = 15;

    fn stream_rlp(&self, stream: &mut RlpStream, truncated: bool) {
        stream.begin_list(Self::NUM_FIELDS - if truncated { 2 } else { 0 });

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
            extra_data: serialized.val_at(12)?,
            mix_hash: serialized.val_at(13)?,
            nonce: serialized.val_at(14)?,
        };

        return Ok(block_header);
    }
}

impl ExtraData {
    pub fn as_slice(&self) -> &[u8] {
        &self.bytes[0..self.len as _]
    }
    pub fn as_mut(&mut self) -> &mut [u8] {
        &mut self.bytes[0..self.len as _]
    }
    pub fn from_slice(data: &[u8]) -> Self {
        assert!(data.len() <= EXTRA_DATA_MAX_LEN);
        let mut ret = Self {
            len: data.len() as _,
            bytes: unsafe { ::std::mem::uninitialized() },
        };
        ret.as_mut().copy_from_slice(data);
        ret
    }
}

impl Encodable for ExtraData {
    fn rlp_append(&self, stream: &mut RlpStream) {
        self.as_slice().rlp_append(stream);
    }
}

impl Decodable for ExtraData {
    fn decode(serialized: &Rlp) -> Result<Self, DecoderError> {
        let v = Vec::<u8>::decode(serialized)?;
        Ok(Self::from_slice(&*v))
    }
}

impl PartialEq for ExtraData {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for ExtraData {}
