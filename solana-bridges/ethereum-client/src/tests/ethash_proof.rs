// Code taken from https://github.com/near/rainbow-bridge

//use crate::{DoubleNodeWithMerkleProof, EthClient};
use arrayref::mut_array_refs;
use ethereum_types::{H128, H256, H512};
use hex::FromHex;
use serde::{Deserialize, Deserializer};
use std::path::Path;

#[derive(Debug)]
struct Hex(pub Vec<u8>);

impl<'de> Deserialize<'de> for Hex {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        let mut s: String = Deserialize::deserialize(deserializer)?;
        if s.starts_with("0x") {
            s = s[2..].to_string();
        }
        if s.len() % 2 == 1 {
            s.insert_str(0, "0");
        }
        Ok(Hex(Vec::from_hex(&s).map_err(|err| {
            serde::de::Error::custom(err.to_string())
        })?))
    }
}

#[derive(Debug, Deserialize)]
struct RootsCollectionRaw {
    pub dag_merkle_roots: Vec<Hex>, // H128
}

#[derive(Debug, Deserialize)]
pub struct RootsCollection {
    pub dag_merkle_roots: Vec<H128>,
}

trait HashExt {
    fn from_slice_extend(s: &[u8]) -> Self;
}

// Also handles converting the endianness
macro_rules! impl_from_slice_extend {
    ( $name:ident ) => {
        impl HashExt for $name {
            fn from_slice_extend(s: &[u8]) -> $name {
                let mut res = $name::zero();
                assert!(s.len() <= $name::len_bytes());
                res.0[$name::len_bytes() - s.len()..].copy_from_slice(s);
                res
            }
        }
    };
}

impl_from_slice_extend! { H128 }
impl_from_slice_extend! { H256 }

impl From<RootsCollectionRaw> for RootsCollection {
    fn from(item: RootsCollectionRaw) -> Self {
        Self {
            dag_merkle_roots: item
                .dag_merkle_roots
                .iter()
                .map(|e| H128::from_slice_extend(&*e.0))
                .collect(),
        }
    }
}

#[derive(Debug, Deserialize)]
struct BlockWithProofsRaw {
    pub proof_length: u64,
    pub header_rlp: Hex,
    pub merkle_root: Hex,        // H128
    pub elements: Vec<Hex>,      // H256
    pub merkle_proofs: Vec<Hex>, // H128
}

#[derive(Debug, Deserialize)]
pub struct BlockWithProofs {
    pub header_rlp: Vec<u8>,
    pub merkle_root: H128,
    pub elements: Vec<H256>,
    pub merkle_proofs: Vec<Vec<H128>>,
}

impl From<BlockWithProofsRaw> for BlockWithProofs {
    fn from(item: BlockWithProofsRaw) -> Self {
        Self {
            header_rlp: item.header_rlp.0,
            merkle_root: H128::from_slice_extend(&*item.merkle_root.0),
            elements: item
                .elements
                .iter()
                .map(|e| H256::from_slice_extend(&*e.0))
                .collect(),
            merkle_proofs: item
                .merkle_proofs
                .chunks(item.proof_length as usize)
                .map(|s| s.iter().map(|e| H128::from_slice_extend(&*e.0)).collect())
                .collect(),
        }
    }
}

fn combine_dag_h256_to_h512<'a>(elements: &'a [H256]) -> impl Iterator<Item = H512> + 'a {
    elements
        .iter()
        .zip(elements.iter().skip(1))
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, (a, b))| {
            let mut buffer = H512::zero();
            {
                let (a_r, b_r) = mut_array_refs!(&mut buffer.0, 32, 32);
                *a_r = a.0;
                *b_r = b.0;
                a_r.reverse();
                b_r.reverse();
            }
            buffer
        })
}

impl BlockWithProofs {
    pub fn elements_512<'a>(&'a self) -> impl Iterator<Item = H512> + 'a {
        combine_dag_h256_to_h512(&*self.elements)
    }

    //pub fn to_double_node_with_merkle_proof_vec(&self) -> Vec<DoubleNodeWithMerkleProof> {
    //    let h512s = Self::combine_dag_h256_to_h512(self.elements.clone());
    //    h512s
    //        .iter()
    //        .zip(h512s.iter().skip(1))
    //        .enumerate()
    //        .filter(|(i, _)| i % 2 == 0)
    //        .map(|(i, (a, b))| DoubleNodeWithMerkleProof {
    //            dag_nodes: vec![*a, *b],
    //            proof: self.merkle_proofs
    //                [i / 2 * self.proof_length as usize..(i / 2 + 1) * self.proof_length as usize]
    //                .to_vec(),
    //        })
    //        .collect()
    //}
}

//// Wish to avoid this code and use web3+rlp libraries directly
//fn rlp_append<TX>(header: &Block<TX>, stream: &mut RlpStream) {
//    stream.begin_list(15);
//    stream.append(&header.parent_hash);
//    stream.append(&header.uncles_hash);
//    stream.append(&header.author);
//    stream.append(&header.state_root);
//    stream.append(&header.transactions_root);
//    stream.append(&header.receipts_root);
//    stream.append(&header.logs_bloom);
//    stream.append(&header.difficulty);
//    stream.append(&header.number.unwrap());
//    stream.append(&header.gas_limit);
//    stream.append(&header.gas_used);
//    stream.append(&header.timestamp);
//    stream.append(&header.extra_data.0);
//    stream.append(&header.mix_hash.unwrap());
//    stream.append(&header.nonce.unwrap());
//}

pub fn read_block(filename: &Path) -> BlockWithProofs {
    read_block_raw(filename).into()
}

fn read_block_raw(filename: &Path) -> BlockWithProofsRaw {
    serde_json::from_reader(std::fs::File::open(filename).unwrap()).unwrap()
}
