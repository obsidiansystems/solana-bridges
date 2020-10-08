use ethereum_types;
use rlp::Rlp;
use crate::eth::*;

macro_rules! arr_declare_wrapper {
    ($name: ident, $len: expr) => {
        #[derive(Eq, PartialEq, Debug)]
        pub struct $name(pub ethereum_types::$name);

        impl From<&[u8; $len]> for $name {
            fn from(item: &[u8; $len]) -> Self {
                $name(item.into())
            }
        }

        impl From<&Vec<u8>> for $name {
            fn from(item: &Vec<u8>) -> Self {
                let mut data = [0u8; $len];
                for i in 0..item.len() {
                    data[$len - 1 - i] = item[item.len() - 1 - i];
                }
                $name(data.into())
            }
        }

        impl From<Vec<u8>> for $name {
            fn from(item: Vec<u8>) -> Self {
                (&item).into()
            }
        }
    };
}

arr_declare_wrapper!(H256, 32);


fn extract_nibbles(a: Vec<u8>) -> Vec<u8> {
    a.iter().flat_map(|b| vec![b >> 4, b & 0x0F]).collect()
}

fn concat_nibbles(a: Vec<u8>) -> Vec<u8> {
    a.iter()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .zip(a.iter().enumerate().filter(|(i, _)| i % 2 == 1))
        .map(|((_, x), (_, y))| (x << 4) | y)
        .collect()
}

pub fn verify_trie_proof(
    expected_root: ethereum_types::H256,
    key: Vec<u8>,
    proof: Vec<Vec<u8>>,
    expected_value: Vec<u8>,
) -> bool {
    let mut actual_key = vec![];
    for el in key {
        if actual_key.len() + 1 == proof.len() {
            actual_key.push(el);
        } else {
            actual_key.push(el / 16);
            actual_key.push(el % 16);
        }
    }
    _verify_trie_proof(H256(expected_root), actual_key, proof, 0, 0, expected_value)
}

pub fn _verify_trie_proof(
    expected_root: H256,
    key: Vec<u8>,
    proof: Vec<Vec<u8>>,
    key_index: usize,
    proof_index: usize,
    expected_value: Vec<u8>,
) -> bool {
    let node = &proof[proof_index];
    let dec = Rlp::new(&node.as_slice());

    if key_index == 0 {
        // trie root is always a hash
        assert_eq!(H256(keccak256(node)), expected_root);
    } else if node.len() < 32 {
        // if rlp < 32 bytes, then it is not hashed
        assert_eq!(dec.as_raw(), expected_root.0.to_fixed_bytes());
    } else {
        assert_eq!(H256(keccak256(node)), expected_root);
    }

    if dec.iter().count() == 17 {
        // branch node
        if key_index == key.len() {
            if dec
                .at(dec.iter().count() - 1)
                .unwrap()
                .as_val::<Vec<u8>>()
                .unwrap()
                == expected_value
            {
                // value stored in the branch
                return true;
            }
        } else if key_index < key.len() {
            let new_expected_root = dec
                .at(key[key_index] as usize)
                .unwrap()
                .as_val::<Vec<u8>>()
                .unwrap();
            if new_expected_root.len() != 0 {
                return _verify_trie_proof(
                    new_expected_root.into(),
                    key,
                    proof,
                    key_index + 1,
                    proof_index + 1,
                    expected_value,
                );
            }
        } else {
            panic!("This should not be reached if the proof has the correct format");
        }
    } else if dec.iter().count() == 2 {
        // leaf or extension node
        // get prefix and optional nibble from the first byte
        let nibbles = extract_nibbles(dec.at(0).unwrap().as_val::<Vec<u8>>().unwrap());
        let (prefix, nibble) = (nibbles[0], nibbles[1]);

        if prefix == 2 {
            // even leaf node
            let key_end = &nibbles[2..];
            if concat_nibbles(key_end.to_vec()) == &key[key_index..]
                && expected_value == dec.at(1).unwrap().as_val::<Vec<u8>>().unwrap()
            {
                return true;
            }
        } else if prefix == 3 {
            // odd leaf node
            let key_end = &nibbles[2..];
            if nibble == key[key_index]
                && concat_nibbles(key_end.to_vec()) == &key[key_index + 1..]
                && expected_value == dec.at(1).unwrap().as_val::<Vec<u8>>().unwrap()
            {
                return true;
            }
        } else if prefix == 0 {
            // even extension node
            let shared_nibbles = &nibbles[2..];
            let extension_length = shared_nibbles.len();
            if concat_nibbles(shared_nibbles.to_vec())
                == &key[key_index..key_index + extension_length]
            {
                let new_expected_root = dec.at(1).unwrap().as_val::<Vec<u8>>().unwrap();
                return _verify_trie_proof(
                    new_expected_root.into(),
                    key,
                    proof,
                    key_index + extension_length,
                    proof_index + 1,
                    expected_value,
                );
            }
        } else if prefix == 1 {
            // odd extension node
            let shared_nibbles = &nibbles[2..];
            let extension_length = 1 + shared_nibbles.len();
            if nibble == key[key_index]
                && concat_nibbles(shared_nibbles.to_vec())
                == &key[key_index + 1..key_index + extension_length]
            {
                let new_expected_root = dec.at(1).unwrap().as_val::<Vec<u8>>().unwrap();
                return _verify_trie_proof(
                    new_expected_root.into(),
                    key,
                    proof,
                    key_index + extension_length,
                    proof_index + 1,
                    expected_value,
                );
            }
        } else {
            panic!("This should not be reached if the proof has the correct format");
        }
    } else {
        panic!("This should not be reached if the proof has the correct format");
    }

    expected_value.len() == 0
}
