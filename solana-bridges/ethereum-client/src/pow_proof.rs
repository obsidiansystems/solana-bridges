use std::mem;

pub use ethereum_types::{H512, U256};

use solana_sdk::{info, program_error::ProgramError};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::eth::BlockHeader;

#[derive(Debug, Eq, PartialEq, Clone, Copy, RlpEncodableDerive, RlpDecodableDerive)]
pub struct AccessedElement {
    pub address: u32,
    pub value: H512,
}

// factored array to avoid fixed size array trait limits
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct AccessedElements(pub [[AccessedElement; 4]; 32]);

impl std::ops::Index<u8> for AccessedElements {
    type Output = AccessedElement;
    fn index(&self, n: u8) -> &Self::Output {
        &self.0[(n / 4) as usize][(n % 4) as usize]
    }
}

impl std::ops::IndexMut<u8> for AccessedElements {
    fn index_mut(&mut self, n: u8) -> &mut Self::Output {
        &mut self.0[(n / 4) as usize][(n % 4) as usize]
    }
}

// These impls flatten it into a [..; 128]

impl rlp::Encodable for AccessedElements {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(128);
        for x in &self.0 {
            for e in x {
                stream.begin_list(2);
                stream.append(&e.address);
                stream.append(&e.value);
            }
        }
    }
}

impl rlp::Decodable for AccessedElements {
    fn decode(serialized: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let mut s: Self = AccessedElements(unsafe { ::std::mem::uninitialized() });
        let mut i = serialized.iter();
        for x in s.0.iter_mut() {
            for y in x.iter_mut() {
                let s2 = i.next().ok_or(rlp::DecoderError::RlpIsTooShort)?;
                *y = {
                    let mut j = s2.iter();
                    let address = j.next().ok_or(rlp::DecoderError::RlpIsTooShort)?.as_val()?;
                    let value = j.next().ok_or(rlp::DecoderError::RlpIsTooShort)?.as_val()?;
                    if j.next().is_some() {
                        Err(rlp::DecoderError::RlpIsTooBig)?;
                    }
                    AccessedElement {
                        address,
                        value,
                    }
                };
            }
        }
        if i.next().is_some() {
            Err(rlp::DecoderError::RlpIsTooBig)?;
        }
        Ok(s)
    }
}
