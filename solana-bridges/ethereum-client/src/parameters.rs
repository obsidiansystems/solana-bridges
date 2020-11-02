use std::mem;

use solana_sdk::{info, program_error::ProgramError};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::eth::{AccessedElements, BlockHeader, U256};

pub const BLOCKS_OFFSET: usize = mem::size_of::<usize>() + mem::size_of::<u64>() + 8; // TODO better
pub const MIN_BUF_SIZE: usize = BLOCKS_OFFSET + mem::size_of::<RingItem>();

pub const STORAGE_ALIGN: usize = std::mem::align_of::<StorageScrach>();

#[derive(Debug, RlpDecodableDerive, RlpEncodableDerive)]
pub struct RingItem {
    pub total_difficulty: U256,
    pub header: BlockHeader,
    pub elements: AccessedElements,
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

fn guard_sufficient_storage(account: &[u8]) -> Result<(), ProgramError> {
    if MIN_BUF_SIZE > account.len() {
        info!("Account data length too small for holding state");
        return Err(ProgramError::AccountDataTooSmall);
    }
    Ok(())
}

#[inline]
pub fn interp(raw_data: &[u8]) -> Result<&Storage, ProgramError> {
    guard_sufficient_storage(raw_data)?;
    let raw_len = raw_data.len();
    let block_len = raw_data[BLOCKS_OFFSET..].len() / mem::size_of::<RingItem>();
    let hacked_data = &raw_data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &Storage = unsafe { std::mem::transmute(hacked_data) };
    // because no stride != size
    debug_assert!(
        std::mem::size_of_val(res) <= (raw_len + STORAGE_ALIGN - 1 / STORAGE_ALIGN) * STORAGE_ALIGN
    );
    debug_assert_eq!(res.headers.len(), block_len);
    Ok(res)
}

#[inline]
pub fn interp_mut(raw_data: &mut [u8]) -> Result<&mut Storage, ProgramError> {
    guard_sufficient_storage(raw_data)?;
    let raw_len = raw_data.len();
    let block_len = raw_data[BLOCKS_OFFSET..].len() / mem::size_of::<RingItem>();
    let hacked_data = &mut raw_data[..block_len];
    // FIXME use proper DST stuff once it exists
    let res: &mut Storage = unsafe { std::mem::transmute(hacked_data) };
    // because no stride != size
    debug_assert!(
        std::mem::size_of_val(res) <= (raw_len + STORAGE_ALIGN - 1 / STORAGE_ALIGN) * STORAGE_ALIGN
    );
    debug_assert_eq!(res.headers.len(), block_len);
    Ok(res)
}

pub fn min_height(data: &Storage) -> u64 {
    let len = data.headers.len();
    match *data {
        Storage {
            full: false,
            offset,
            ..
        } => data.height - offset as u64 + 1,
        Storage { full: true, .. } => data.height - len as u64 + 1,
    }
}

pub fn lowest_offset(data: &Storage) -> usize {
    match *data {
        Storage { full: false, .. } => 0,
        Storage {
            full: true, offset, ..
        } => offset,
    }
}

pub fn read_block<'a>(data: &'a Storage, idx: usize) -> Result<Option<&'a RingItem>, ProgramError> {
    let len = data.headers.len();
    match *data {
        Storage {
            full: false,
            offset,
            ..
        } if idx < offset => (),
        Storage { full: true, .. } if idx < len => (),
        _ => return Ok(None),
    };
    assert!(data.height != 0);
    let ref header = data.headers[idx];
    Ok(Some(header))
}

pub fn read_prev_block<'a>(data: &'a Storage) -> Result<Option<&'a RingItem>, ProgramError> {
    let len = data.headers.len();
    read_block(data, (data.offset + (len - 1)) % len)
}

pub fn write_new_block_unvalidated(
    data: &mut Storage,
    header: &BlockHeader,
    old_total_difficulty_opt: Option<&U256>,
    elems: &AccessedElements,
) -> Result<(), ProgramError> {
    let old_offset = data.offset;

    let total_difficulty = match old_total_difficulty_opt {
        Some(&d) => d,
        None => match read_prev_block(data)? {
            None => U256::zero(),
            Some(prev_item) => prev_item.total_difficulty + header.difficulty,
        },
    };

    data.headers[old_offset] = RingItem {
        header: header.clone(),
        total_difficulty,
        elements: *elems,
    };

    data.height = header.number;
    data.offset = (old_offset + 1) % data.headers.len();
    data.full |= data.offset <= old_offset;

    return Ok(());
}
