use arrayref::{
    array_ref,
    mut_array_refs,
};

use ethereum_types::{H128, H256, H512};

use solana_sdk::hash::hash as sha256;

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::{
    eth::*,
    epoch_roots::EPOCH_ROOTS,
    instruction::*,
    ledger_ring_buffer::*,
};

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

pub fn apply_pow_element_merkle_proof(elems: &ElementPair, merkle_spine: &[H128], index: u32) -> H128 {

    fn truncate_to_h128(arr: H256) -> H128 {
        H128(*array_ref!(&arr.0, 16, 16))
    }

    fn hash_h128(l: H128, r: H128) -> H128 {
        let mut data = [0u8; 64];
        let (_, l_dst, _, r_dst) = mut_array_refs!(&mut data, 16, 16, 16, 16);
        *l_dst = l.0;
        *r_dst = r.0;
        truncate_to_h128(H256(sha256(&data).0))
    }

    let mut accum = {
        let mut data = [0u8; 128];
        {
            let (low_dst, high_dst) = mut_array_refs!(&mut data, 64, 64);
            *low_dst  = elems.e0.0;
            *high_dst = elems.e1.0;
        }
        truncate_to_h128(sha256(&data).0.into())
    };

    for (i, &sibling) in merkle_spine.iter().enumerate() {
        if (index >> i as u64) % 2 == 0 {
            accum = hash_h128(accum, sibling);
        } else {
            accum = hash_h128(sibling, accum);
        }
    }
    accum
}

pub fn check_pow_element_merkle_proof(height: u64, elems: &ElementPair, merkle_spine: &[H128], index: u32) -> bool {
    let merkel_root = EPOCH_ROOTS[height_to_epoch(height) as usize];

    let calculated_root = apply_pow_element_merkle_proof(elems, merkle_spine, index);

    calculated_root == merkel_root
}

pub fn verify_pow_indexes(ri: &mut RingItem) -> bool {
    let mut iter = ri.elements.0.iter_mut().flat_map(|x| x.iter_mut());
    verify_pow(&ri.header, |wanted_addr| {
        let a = iter.next().unwrap();
        // Set for challengers, now that we know what it is.
        a.address = wanted_addr;
        a.value
    })
}
