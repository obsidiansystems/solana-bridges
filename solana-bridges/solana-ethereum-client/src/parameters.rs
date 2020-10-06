use std::mem;

pub const HEADER_HISTORY_SIZE: usize = 100;

pub const COUNT_OFFSET: usize = 0;
pub const COUNT_SIZE: usize = mem::size_of::<u64>();
pub const HEIGHT_OFFSET: usize = COUNT_SIZE;
pub const HEIGHT_SIZE: usize = mem::size_of::<u64>();
pub const BLOCKS_OFFSET: usize = COUNT_SIZE + HEIGHT_SIZE;
