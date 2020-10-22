use std::mem;

use solana_sdk::{
    program_error::ProgramError,
    program_pack::{Pack, Sealed},
};

use arrayref::{array_mut_ref, array_ref, array_refs, mut_array_refs};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::eth::{U256, BlockHeader};


pub const BLOCKS_OFFSET: usize = mem::size_of::<usize>() + mem::size_of::<u64>() + 1; // TODO better
pub const MIN_BUF_SIZE: usize = BLOCKS_OFFSET + RingItem::LEN;

pub const STORAGE_ALIGN: usize = std::mem::align_of::<StorageScrach>();

#[repr(C)]
pub struct StorageT<X: ?Sized> {
    pub height: u64,
    pub offset: usize,
    pub full: bool,
    pub headers: X,
}

pub type Storage = StorageT<[[u8; RingItem::LEN]]>;

// Something sized that can be unsized, useful for some compile time math
pub type StorageScrach = StorageT<[[u8; RingItem::LEN]; 5]>;

#[derive(RlpDecodableDerive, RlpEncodableDerive)]
pub struct RingItem {
    pub total_difficulty: U256,
    pub header: BlockHeader,
}

impl Sealed for RingItem {}
impl Pack for RingItem {
    const LEN: usize = mem::size_of::<U256>() + BlockHeader::LEN;
    fn pack_into_slice(&self, dst: &mut [u8]) {
        let dst = array_mut_ref![dst, 0, RingItem::LEN];
        let (
            total_difficulty_dst,
            header_dst,
        ) = mut_array_refs![
            dst,
            mem::size_of::<U256>(),
            BlockHeader::LEN
        ];
        self.total_difficulty.to_little_endian(total_difficulty_dst);
        self.header.pack_into_slice(header_dst);
    }
    fn unpack_from_slice(src: &[u8]) -> Result<Self, ProgramError> {
        let src = array_ref![src, 0, RingItem::LEN];
        let (
            total_difficulty_src,
            header_src,
        ) = array_refs![
            src,
            mem::size_of::<U256>(),
            BlockHeader::LEN
        ];
        Ok(RingItem {
            total_difficulty: U256::from_little_endian(total_difficulty_src),
            header: BlockHeader::unpack_from_slice(header_src)?,
        })
    }
}
