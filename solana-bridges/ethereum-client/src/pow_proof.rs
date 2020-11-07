use std::mem;

pub use ethereum_types::U256;

use solana_sdk::{info, program_error::ProgramError};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::eth::{BlockHeader, U256};

#[derive(Debug, Eq, PartialEq, Clone, Copy, RlpEncodableDerive, RlpDecodableDerive)]
pub struct AccessedElement {
    pub address: u32,
    pub value: H512,
}

// factored array to avoid fixed size array trait limits
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct AccessedElements(pub [[AccessedElement; 4]; 32]);

// These impls flatten it into a [..; 128]

impl rlp::Encodable for AccessedElements {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(128);
        for x in &self.0 {
            for &AccessedElement(ref k, ref v) in x {
                stream.begin_list(2);
                stream.append(k);
                stream.append(v);
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
                    let idx = j.next().ok_or(rlp::DecoderError::RlpIsTooShort)?.as_val()?;
                    let v = j.next().ok_or(rlp::DecoderError::RlpIsTooShort)?.as_val()?;
                    if j.next().is_some() {
                        Err(rlp::DecoderError::RlpIsTooBig)?;
                    }
                    AccessedElement(idx, v)
                };
            }
        }
        if i.next().is_some() {
            Err(rlp::DecoderError::RlpIsTooBig)?;
        }
        Ok(s)
    }
}
