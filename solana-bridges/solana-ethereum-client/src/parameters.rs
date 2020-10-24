use std::mem;

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::eth::{U256, BlockHeader};


pub const BLOCKS_OFFSET: usize = mem::size_of::<usize>() + mem::size_of::<u64>() + 8; // TODO better
pub const MIN_BUF_SIZE: usize = BLOCKS_OFFSET + mem::size_of::<RingItem>();

pub const STORAGE_ALIGN: usize = std::mem::align_of::<StorageScrach>();

#[derive(Debug)]
#[derive(RlpDecodableDerive, RlpEncodableDerive)]
pub struct RingItem {
    pub total_difficulty: U256,
    pub header: BlockHeader,
}

#[derive(Debug)]
#[repr(C)]
pub struct StorageT<X: ?Sized> {
    pub height: u64,
    pub offset: usize,
    pub full: bool,
    pub headers: X,
}

pub type Storage = StorageT<[RingItem]>;

// Something sized that can be unsized, useful for some compile time math
pub type StorageScrach = StorageT<[RingItem; 5]>;
