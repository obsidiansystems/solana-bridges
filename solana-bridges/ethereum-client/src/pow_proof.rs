use arrayref::{
    array_ref,
    mut_array_refs,
};

use ethereum_types::{H128, H512};

use solana_sdk::hash::hash as sha256;

use rlp_derive::{RlpDecodable as RlpDecodableDerive, RlpEncodable as RlpEncodableDerive};

use crate::{
    eth::*,
    epoch_roots::EPOCH_ROOTS,
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


pub fn hash_h128(arr: &[u8]) -> H128 {
    let data = sha256(arr).0;
    H128(*array_ref!(&data, 16, 16))
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, RlpEncodableDerive, RlpDecodableDerive)]
pub struct ElementPair {
    pub e0: H512,
    pub e1: H512,
}

impl ElementPair {
    pub fn reduce(&self) -> H128 {
        let mut data = [0u8; 128];
        {
            {
                let (a_dst, b_dst) = mut_array_refs!(&mut data, 64, 64);
                *a_dst = self.e0.0;
                *b_dst = self.e1.0;
            }
            {
                let (a_dst, b_dst, c_dst, d_dst) = mut_array_refs!(&mut data, 32, 32, 32, 32);
                a_dst.reverse();
                b_dst.reverse();
                c_dst.reverse();
                d_dst.reverse();
            }
        }
        hash_h128(&data)
    }

}

pub fn combine_h128(l: H128, r: H128) -> H128 {
    let mut data = [0u8; 64];
    let (_, l_dst, _, r_dst) = mut_array_refs!(&mut data, 16, 16, 16, 16);
    *l_dst = l.0;
    *r_dst = r.0;

    hash_h128(&data)
}

pub fn apply_pow_element_merkle_proof(elems: &ElementPair, merkle_spine: &[H128], mut index: u32) -> H128 {
    index /= 2; // because we are looking at a pair of elements.

    let mut accum = elems.reduce();

    for (i, &sibling) in merkle_spine.iter().enumerate() {
        if (index >> i as u64) % 2 == 0 {
            accum = combine_h128(accum, sibling);
        } else {
            accum = combine_h128(sibling, accum);
        }
    }

    accum
}

pub fn get_wanted_merkle_root(height: u64) -> H128 {
    EPOCH_ROOTS[height_to_epoch(height) as usize]
}

#[cfg(not(target_arch = "bpf"))]
pub fn verify_pow_indexes(ri: &mut RingItem) -> bool {
    let mut iter = ri.elements.0.iter_mut().flat_map(|x| x.iter_mut());
    verify_pow(&ri.header, |wanted_addr| {
        let a = iter.next().unwrap();
        // Set for challengers, now that we know what it is.
        a.address = wanted_addr;
        a.value
    })
}

//TODO: remove once Keccak syscalls are added - currently runs into instruction limit
#[cfg(target_arch = "bpf")]
pub fn verify_pow_indexes(_ri: &mut RingItem) -> bool {
    return true;
}
