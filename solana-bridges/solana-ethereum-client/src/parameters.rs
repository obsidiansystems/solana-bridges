use std::mem;

use solana_sdk::program_pack::{Pack};

use crate::eth::BlockHeader;

pub const HEADER_HISTORY_SIZE: usize = 100;

pub const BLOCKS_OFFSET: usize = 2 * mem::size_of::<u64>(); // TODO better
// + mem::size_of::<u8>();
pub const MIN_BUF_SIZE: usize = BLOCKS_OFFSET + BlockHeader::LEN;

#[repr(C)]
pub struct Storage {
    //pub full: bool,
    pub count: usize,
    pub height: u64,
    pub headers: [[u8; BlockHeader::LEN]],
}
