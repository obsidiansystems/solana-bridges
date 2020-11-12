pub use ethereum_types::{H512, U256};

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

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
