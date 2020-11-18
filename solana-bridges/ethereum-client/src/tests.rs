use quickcheck_macros::quickcheck;

use crate::{
    instruction::*,
    ledger_ring_buffer::*,
    pow_proof::*,
    processor::*,
    types::*,
    eth::*,
    prove::*,
};

use std::{cell::RefCell, ops::Deref, path::Path, rc::Rc, str::FromStr};

use solana_sdk::{account_info::AccountInfo, program_error::ProgramError, pubkey::Pubkey};

use ethereum_types::{Bloom, H160, H256, H64, H512, U256};
use rlp::{Decodable, DecoderError, Rlp, RlpStream};
use solana_sdk::clock::Epoch;

mod ethash_proof;

mod blocks;
mod relayer_runs;
use blocks::*;

mod inclusion;

// Required to support info! in tests
#[cfg(not(target_arch = "bpf"))]
solana_sdk::program_stubs!();

const THIS_PROG_ID: Pubkey = Pubkey::new_from_array([
    0x0B, 0xCE, 0xDE, 0xAF,
    0xCE, 0xCD, 0xEF, 0xFF,

    0x0B, 0xCE, 0xDE, 0xAF,
    0xCE, 0xCD, 0xEF, 0xFF,

    0x0B, 0xCE, 0xDE, 0xAF,
    0xCE, 0xCD, 0xEF, 0xFF,

    0x0B, 0xCE, 0xDE, 0xAF,
    0xCE, 0xCD, 0xEF, 0xFF,
]);

#[test]
fn headers_offset_correct() -> Result<(), TestError> {
    let p0 = 0 as *const StorageScrach;
    let p1 = p0 as *const Storage;
    let p2 = unsafe { &(*p1).headers[0] as *const _ };
    let offset = p2 as usize - p0 as usize;
    assert_eq!(offset, BLOCKS_OFFSET);
    Ok(())
}

#[test]
fn block_construction() -> Result<(), TestError> {
    let header_400000 = decode_rlp(HEADER_400000)?;
    let block_400000 = Block {
        header: header_400000,
        transactions: Vec::new(),
    };
    assert_eq!(block_400000.transactions.len(), 0);
    Ok(())
}

#[test]
fn test_instructions_0() -> Result<(), TestError> {
    test_instructions(20, 30)?;
    Ok(())
}


#[ignore]
#[quickcheck]
fn test_instructions_quickcheck(buf_len: usize, block_count: usize) -> Result<(), TestError> {
    test_instructions(buf_len, block_count)?;
    Ok(())
}

fn ethash_element_chunks(height: u64, block: &ethash_proof::BlockWithProofs) -> Vec<ProvidePowElement> {
    let mut elems = block.elements_512();
    let mut out = Vec::new();

    'outer: for i in 0.. {
        let mut prov = ProvidePowElement::new(height, i);

        for j in 0..ProvidePowElement::ETHASH_ELEMENTS_PER_INSTRUCTION {
            match elems.next() {
                None => break 'outer,
                Some(h) => prov.elements[j as usize] = h,
            };
        }
        out.push(prov);
    }
    return out;
}

fn test_instructions(mut buf_len: usize, mut block_count: usize) -> Result<Vec<u8>, TestError> {
    buf_len *= std::mem::size_of::<RingItem>() / 7;
    buf_len += MIN_BUF_SIZE;

    block_count += 1;
    block_count = std::cmp::min(block_count, 5);

    let mut raw_data = vec![0; buf_len];

    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        let instruction_noop: Vec<u8> = Instruction::Noop.pack();
        process_instruction(&THIS_PROG_ID, &accounts, &instruction_noop)
            .map_err(TestError::ProgError)?;

        let dir = Path::new(file!())
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("data/ethash-proof");
        {
            let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
                let mut data = dir.clone();
                data.push("mainnet-400000.json");
                data
            });
            {
                let header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
                let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                    header: Box::new(header_400000),
                    total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for 1now
                }))
                .pack();
                process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
                    .map_err(TestError::ProgError)?;
            }

            for ppe in ethash_element_chunks(400_000, &block_with_proofs) {
                let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
                    .pack();
                process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
                    .map_err(TestError::ProgError)?;
            }
        }

        for n in 1..block_count {
            let height = 400_000 + n as u64;
            let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
                let mut data = dir.clone();
                data.push(&*format!("mainnet-400{:03}.json", n));
                data
            });
            {
                let raw_data = accounts[0]
                    .try_borrow_data()
                    .map_err(TestError::ProgError)?;
                let data = interp(&*raw_data).map_err(TestError::ProgError)?;
                println!(
                    "ring size: {}, full: {}, current block short no: {}",
                    data.headers.len(),
                    data.full,
                    n
                );
            }
            {
                let header_4000xx: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
                let instruction_new: Vec<u8> = Instruction::NewBlock(Box::new(header_4000xx))
                    .pack();
                process_instruction(&THIS_PROG_ID, &accounts, &instruction_new)
                    .map_err(TestError::ProgError)?;
            }

            for ppe in ethash_element_chunks(height, &block_with_proofs) {
                let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
                    .pack();
                process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
                    .map_err(TestError::ProgError)?;
            }
        }

        let raw_data = accounts[0]
            .try_borrow_data()
            .map_err(TestError::ProgError)?;
        let data = interp(&*raw_data).map_err(TestError::ProgError)?;

        assert_eq!(block_count % data.headers.len(), data.offset);
        assert_eq!(400000 - 1 + block_count as u64, data.height);
        assert_eq!(block_count >= data.headers.len(), data.full);

        Ok(())
    })?;

    Ok(raw_data)
}

pub fn verify_pow_from_scratch(header: &BlockHeader) -> (bool, Vec<(u32, H512)>) {
    use ethash::*;
    let epoch = height_to_epoch(header.number) as usize;
    let seed = get_seedhash(epoch);
    let cache_size = get_cache_size(epoch);

    let mut cache = vec![0; cache_size];
    make_cache(&mut cache, seed); //TODO: hits maximum instructions limit

    let mut v = Vec::new();

    let res = verify_pow(header, |i| {
        let r = calc_dataset_item(&cache, i);
        v.push((i, r));
        r
    });

    (res, v)
}

// Slow tests ~ 1min each without cache sharing

#[ignore]
#[test]
fn test_pow_0() -> Result<(), TestError> {
    let mut header_400000: BlockHeader = decode_rlp(HEADER_400000)?;
    assert!(verify_pow_from_scratch(&header_400000).0);
    header_400000.nonce = H64::zero();
    assert!(!verify_pow_from_scratch(&header_400000).0);
    Ok(())
}

#[ignore]
#[test]
fn test_pow_1() -> Result<(), TestError> {
    assert!(verify_pow_from_scratch(&decode_rlp(HEADER_400001)?).0);
    Ok(())
}

#[ignore]
#[test]
fn test_pow_2() -> Result<(), TestError> {
    assert!(verify_pow_from_scratch(&decode_rlp(HEADER_8996776)?).0);
    Ok(())
}

#[ignore]
#[test]
fn dump_entries() -> Result<(), TestError> {
    let x = verify_pow_from_scratch(&decode_rlp(HEADER_400000)?).1;
    let mut res = String::new();
    for (k, v) in x {
        res += &*std::format!("        ({:#8x}, hex!(\"{:#x}\"))\n", k, v);
    }
    panic!("{}", res);
}

#[test]
fn test_roundtrip_rlp_header() -> Result<(), TestError> {
    let expected = decoded_header_0()?;
    assert_eq!(expected, decode_rlp(&rlp::encode(&expected))?);
    return Ok(());
}

#[quickcheck]
fn test_rlp_initialize(w0: u64, w1: u64, w2: u64, w3: u64) -> Result<(), TestError> {
    let expected = Initialize {
        total_difficulty: Box::new(U256([w0, w1, w2, w3])),
        header: Box::new(decoded_header_0()?),
    };
    let rlp = {
        let mut s = RlpStream::new();
        s.begin_list(2);
        s.append(&*expected.total_difficulty);
        s.append(&*expected.header);
        s.out()
    };
    assert_eq!(expected, decode_rlp(&rlp)?);
    assert_eq!(&rlp, &rlp::encode(&expected));
    return Ok(());
}

#[quickcheck]
fn test_rlp_extra_data(mut data: Vec<u8>) -> Result<(), TestError> {
    data.truncate(32);
    let data2: Vec<u8> = decode_rlp(&rlp::encode(&data))?;
    assert_eq!(data, data2);
    Ok(())
}

#[test]
fn test_decoding() -> Result<(), TestError> {
    let expected = decoded_header_0()?;
    let header: BlockHeader = decode_rlp(TEST_HEADER_0)?;
    assert_eq!(header, expected);

    let header_400000: BlockHeader = decode_rlp(HEADER_400000)?;
    assert_eq!(header_400000.number, 400000);
    assert_eq!(header_400000.difficulty, U256::from(6022643743806 as u64));
    assert_eq!(
        hash_header(&header_400000, false),
        H256::from_str("5d15649e25d8f3e2c0374946078539d200710afc977cdfc6a977bd23f20fa8e8")
            .map_err(|_| TestError::HexError)?
    );

    let test_block_0_tx: Block = decode_rlp(TEST_BLOCK_0_TX)?;
    assert_eq!(test_block_0_tx.header.number, 4);

    let test_block_1_tx: Block = decode_rlp(TEST_BLOCK_1_TX)?;
    assert_eq!(test_block_1_tx.header.number, 2);
    assert_eq!(test_block_1_tx.transactions.len(), 1);

    return Ok(());
}

#[derive(Debug, PartialEq, Eq)]
pub enum TestError {
    HexError,
    RlpError(DecoderError),
    ProgError(ProgramError),
}

fn decode_rlp<T: Decodable>(bytes: &[u8]) -> Result<T, TestError> {
    let rlp = Rlp::new(bytes);
    return T::decode(&rlp).map_err(TestError::RlpError);
}

pub fn test_inclusion(
    receipt_index: u64,
    receipt_data: &[u8],
    header_data: &[u8],
    proof_data: &[&[&[u8]]],
) -> Result<(), DecoderError> {
    let header: BlockHeader = rlp::decode(header_data).unwrap();

    let proof_vecs: Vec<_> = proof_data
        .iter()
        .map(|&node| {
            let mut stream = RlpStream::new();
            stream.append_list::<&[u8], _>(node);
            stream.out()
        })
        .collect();

    assert!(verify_trie_proof(
        header.receipts_root,
        &*rlp::encode(&receipt_index),
        proof_vecs.iter().map(Deref::deref).map(Ok),
        receipt_data,
    )?);
    Ok(())
}

pub fn pack_proof(proof_data: &[&[&[u8]]]) -> Vec<u8> {
    let proof_vecs: Vec<_> = proof_data
        .iter()
        .map(|&node| {
            let mut stream = RlpStream::new();
            stream.append_list::<&[u8], _>(node);
            stream.out()
        })
        .collect();

    let mut stream = RlpStream::new();
    stream.append_list::<Vec<u8>, _>(&*proof_vecs);
    stream.out()
}

pub fn with_account<K, R>(raw_data: &mut [u8], k: K) -> R
where
    K: FnOnce(AccountInfo) -> R,
{
    let key = Pubkey::default();
    let mut lamports = 0;

    let owner = THIS_PROG_ID;

    k(AccountInfo {
        key: &key,
        is_signer: true,
        is_writable: true,
        lamports: Rc::new(RefCell::new(&mut lamports)),
        data: Rc::new(RefCell::new(raw_data)),
        owner: &owner,
        executable: false,
        rent_epoch: Epoch::default(),
    })
}

#[ignore]
#[test]
fn relayer_run_0() -> Result<(), TestError> {
    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        for instr in &relayer_runs::RUN_0 {
            process_instruction(&THIS_PROG_ID, &accounts, instr).unwrap();
        }

        Ok(())
    })
}

#[ignore]
#[test]
fn relayer_run_1() -> Result<(), TestError> {
    let mut raw_data = vec![0; 1 << 12];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        for instr in &relayer_runs::RUN_1 {
            process_instruction(&THIS_PROG_ID, &accounts, instr).unwrap();
            //let r = accounts[0].data.try_borrow().unwrap();
            //let data = interp(&*r)?;
            //println!("{:#?}", data);
        }

        Ok(())
    })
}

pub fn test_inclusion_instruction<F>(
    header_data: &[u8],
    _elems_raw: &[(u32, H512); 128],
    instruction_fun: F,
) -> Result<(), TestError>
where
    F: FnOnce(BlockHeader) -> ProveInclusion,
{
    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let mut accounts = vec![account];

        let header: BlockHeader = rlp::decode(header_data).unwrap();

        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                total_difficulty: Box::new(U256::zero()),
                header: Box::new(header.clone()),
            }))
            .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_init).unwrap();
        }

        // Skipping because specific test block from rainbow bridge doesn't actually have valid nonce.
        //
        //for &(_, h) in elems_raw as &[(u32, H512)] {
        //    let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ProvidePowElement {
        //        element: Box::new(h),
        //    }))
        //        .pack();
        //    process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
        //        .map_err(TestError::ProgError)?;
        //}

        {
            // Hack ethash_elements so it's as if we did submit the pow
            // elements and the POW passed.
            let mut raw_data = accounts[0]
                .try_borrow_mut_data()
                .map_err(TestError::ProgError)?;
            let ref mut data = *interp_mut(&mut *raw_data).map_err(TestError::ProgError)?;
            data.ethash_elements = ElementChunkSet::READY_FOR_BLOCK;
        }

        {
            accounts[0].is_writable = false;

            let instruction_proove_incl: Vec<u8> =
                Instruction::ProveInclusion(Box::new(instruction_fun(header))).pack();

            process_instruction(&THIS_PROG_ID, &accounts, &instruction_proove_incl)
                .map_err(TestError::ProgError)?;
        }

        Ok(())
    })
}

#[test]
pub fn test_inclusion_0() -> Result<(), DecoderError> {
    use inclusion::test_0::*;
    test_inclusion(RECEIPT_INDEX, RECEIPT_DATA, HEADER_DATA, PROOF_DATA)
}

#[test]
pub fn test_inclusion_1() -> Result<(), DecoderError> {
    use inclusion::test_1::*;
    test_inclusion(RECEIPT_INDEX, RECEIPT_DATA, HEADER_DATA, PROOF_DATA)
}

#[test]
pub fn test_inclusion_instruction_bad_block() -> () {
    use inclusion::test_0::*;
    // note the err() to require an error;
    let res = test_inclusion_instruction(HEADER_DATA, HEADER_POW_ELEMS, |header| {
        let mut block_hash = Box::new(hash_header(&header, false));
        (*block_hash).0[5] += 1;
        ProveInclusion {
            height: header.number,
            block_hash,
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256::zero()),
        }
    });
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_BadBlockHash.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_too_easy() {
    use inclusion::test_0::*;
    // note the err() to require an error;
    let res = test_inclusion_instruction(HEADER_DATA, HEADER_POW_ELEMS, |header: BlockHeader| {
        ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256([9, 9, 9, 9])),
        }
    });
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_TooEasy.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_bad_proof() {
    use inclusion::test_0::*;
    // note the err() to require an error;
    let res = test_inclusion_instruction(HEADER_DATA, HEADER_POW_ELEMS, |header: BlockHeader| {
        let mut proof = pack_proof(PROOF_DATA);
        proof[5] += 1;
        ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof,
            min_difficulty: Box::new(U256::zero()),
        }
    });
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_BadMerkle.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_0() -> Result<(), TestError> {
    use inclusion::test_0::*;
    test_inclusion_instruction(HEADER_DATA, HEADER_POW_ELEMS, |header: BlockHeader| {
        ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256::zero()),
        }
    })
}

#[test]
pub fn test_inclusion_instruction_1() -> Result<(), TestError> {
    use inclusion::test_1::*;
    test_inclusion_instruction(HEADER_DATA, HEADER_POW_ELEMS, |header: BlockHeader| {
        ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256::zero()),
        }
    })
}

#[test]
pub fn test_pow_indices_400000() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut ri = RingItem {
        total_difficulty: U256::zero(),
        header: decode_rlp(&*block_with_proofs.header_rlp)?,
        elements: DUMMY_ELEMS,
    };

    for (i, h) in block_with_proofs.elements_512().enumerate() {
        ri.elements[i as u8].value = h;
    }

    //println!("{:#?}", ri);

    assert!(verify_pow_indexes(&mut ri));

    Ok(())
}

#[test]
pub fn test_pow_element_proof() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400001.json");
        data
    });

    let mut ri = RingItem {
        total_difficulty: U256::zero(),
        header: decode_rlp(&*block_with_proofs.header_rlp)?,
        elements: DUMMY_ELEMS,
    };

    for (i, h) in block_with_proofs.elements_512().enumerate() {
        ri.elements[i as u8] = AccessedElement {
            address: 0x2C7A8A,
            value: h,
        };
    }

    //println!("{:#?}", ri);

    let wanted_merkle_root = get_wanted_merkle_root(ri.header.number);

    let got_merkle_root = apply_pow_element_merkle_proof(
        &ElementPair {
            e0: ri.elements[0].value,
            e1: ri.elements[1].value,
        },
        &*block_with_proofs.merkle_proofs[0],
        ri.elements[0].address);

    assert_eq!(got_merkle_root, wanted_merkle_root);

    Ok(())
}

#[test]
pub fn test_bad_block_caught_with_pow() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        let mut header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;

        // corrupt block, so it will fail PoW later.
        header_400000.timestamp += 5;

        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                header: Box::new(header_400000.clone()),
                total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for now
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
                .map_err(TestError::ProgError)?;
        }

        let mut res = Ok(());
        for ppe in ethash_element_chunks(400_000, &block_with_proofs) {
            let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
                .pack();
            res?;
            res = process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
                .map_err(TestError::ProgError);
        }

        assert_eq!(
            res.err().unwrap(),
            TestError::ProgError(CustomError::VerifyHeaderFailed_InvalidProofOfWork.to_program_error()),
        );

        Ok(())
    })
}

#[test]
pub fn test_bad_challenge_same_elem() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        let header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                header: Box::new(header_400000.clone()),
                total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for now
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
                .map_err(TestError::ProgError)?;
        }

        for ppe in ethash_element_chunks(400_000, &block_with_proofs) {
            let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
                .map_err(TestError::ProgError)?;
        }

        let res = {
            let instruction_chal: Vec<u8> = Instruction::Challenge(Box::new(Challenge {
                height: 400_000,
                block_hash: Box::new(hash_header(&header_400000, false)),
                element_index: 0,
                merkle_spine: block_with_proofs.merkle_proofs[0].clone(),
                element_pair: {
                    let mut iter = block_with_proofs.elements_512();
                    Box::new(ElementPair {
                        e0: iter.next().unwrap(),
                        e1: iter.next().unwrap(),
                    })
                },
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_chal)
                .map_err(TestError::ProgError)
        };

        assert_eq!(
            res.err().unwrap(),
            TestError::ProgError(CustomError::InvalidChallenge_SameElement.to_program_error()),
        );

        Ok(())
    })
}

#[test]
pub fn test_challenge_before_elems() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        let header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                header: Box::new(header_400000.clone()),
                total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for now
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
                .map_err(TestError::ProgError)?;
        }

        let res = {
            let instruction_chal: Vec<u8> = Instruction::Challenge(Box::new(Challenge {
                height: 400_000,
                block_hash: Box::new(hash_header(&header_400000, false)),
                element_index: 0,
                merkle_spine: block_with_proofs.merkle_proofs[0].clone(),
                element_pair: {
                    let mut iter = block_with_proofs.elements_512();
                    Box::new(ElementPair {
                        e0: iter.next().unwrap(),
                        e1: iter.next().unwrap(),
                    })
                },
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_chal)
                .map_err(TestError::ProgError)
        };

        assert_eq!(
            res.err().unwrap(),
            TestError::ProgError(CustomError::BlockNotFound.to_program_error()),
        );

        Ok(())
    })
}

#[test]
pub fn test_challenge_bad_root() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let accounts = vec![account];

        let header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                header: Box::new(header_400000.clone()),
                total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for now
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
                .map_err(TestError::ProgError)?;
        }

        for ppe in ethash_element_chunks(400_000, &block_with_proofs) {
            let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
                .map_err(TestError::ProgError)?;
        }

        const FAKE_HEIGHT: u64 = 999_999;

        let fake_hash = {
            // Hack block hash so it's as if we did submit the pow
            // elements and the POW passed, but challenges will get wrong epoch.
            let mut raw_data = accounts[0]
                .try_borrow_mut_data()
                .map_err(TestError::ProgError)?;
            let ref mut data = *interp_mut(&mut *raw_data).map_err(TestError::ProgError)?;
            data.height = FAKE_HEIGHT;
            let ri = read_prev_block_mut(data)
                .map_err(TestError::ProgError)?
                .unwrap();
            ri.header.number = FAKE_HEIGHT;

            // Also mess up an element
            ri.elements[0].value = H512::zero();

            hash_header(&ri.header, false)
        };

        let res = {
            let instruction_chal: Vec<u8> = Instruction::Challenge(Box::new(Challenge {
                height: FAKE_HEIGHT,
                block_hash: Box::new(fake_hash),
                element_index: 0,
                merkle_spine: block_with_proofs.merkle_proofs[0].clone(),
                element_pair: {
                    let mut iter = block_with_proofs.elements_512();
                    Box::new(ElementPair {
                        e0: iter.next().unwrap(),
                        e1: iter.next().unwrap(),
                    })
                },
            }))
                .pack();
            process_instruction(&THIS_PROG_ID, &accounts, &instruction_chal)
                .map_err(TestError::ProgError)
        };

        assert_eq!(
            res.err().unwrap(),
            TestError::ProgError(CustomError::InvalidChallenge_BadMerkleRoot.to_program_error()),
        );

        Ok(())
    })
}

#[test]
pub fn test_successful_challenge() -> Result<(), TestError> {
    let dir = Path::new(file!())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/ethash-proof");
    let block_with_proofs: ethash_proof::BlockWithProofs = ethash_proof::read_block(&*{
        let mut data = dir.clone();
        data.push("mainnet-400000.json");
        data
    });

    let mut src_raw_data = vec![0; 1 << 16];
    let mut src_lamports = 50_000;
    let src_key = Pubkey::default();
    let src_owner = THIS_PROG_ID;

    let src_account = AccountInfo {
        key: &src_key,
        is_signer: true,
        is_writable: true,
        lamports: Rc::new(RefCell::new(&mut src_lamports)),
        data: Rc::new(RefCell::new(&mut *src_raw_data)),
        owner: &src_owner,
        executable: false,
        rent_epoch: Epoch::default(),
    };

    let mut dst_raw_data = vec![];
    let mut dst_lamports = 0;
    let dst_key = Pubkey::default();
    let dst_owner = Pubkey::default();

    let dst_account = AccountInfo {
        key: &dst_key,
        is_signer: true,
        is_writable: true,
        lamports: Rc::new(RefCell::new(&mut dst_lamports)),
        data: Rc::new(RefCell::new(&mut *dst_raw_data)),
        owner: &dst_owner,
        executable: false,
        rent_epoch: Epoch::default(),
    };

    let accounts = vec![src_account, dst_account];

    let header_400000: BlockHeader = decode_rlp(&*block_with_proofs.header_rlp)?;
    {
        let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
            header: Box::new(header_400000.clone()),
            total_difficulty: Box::new(U256([0, 1, 1, 1])), // arbitrarily chosen number for now
        }))
            .pack();
        process_instruction(&THIS_PROG_ID, &accounts, &instruction_init)
            .map_err(TestError::ProgError)?;
    }

    for ppe in ethash_element_chunks(400_000, &block_with_proofs) {
        let instruction_pow: Vec<u8> = Instruction::ProvidePowElement(Box::new(ppe))
            .pack();
        process_instruction(&THIS_PROG_ID, &accounts, &instruction_pow)
            .map_err(TestError::ProgError)?;
    }

    {
        // mess up an element so challenge will succeed
        let mut raw_data = accounts[0]
            .try_borrow_mut_data()
            .map_err(TestError::ProgError)?;
        let ref mut data = *interp_mut(&mut *raw_data).map_err(TestError::ProgError)?;
        let ri = read_prev_block_mut(data)
            .map_err(TestError::ProgError)?
            .unwrap();
        ri.elements[0].value = H512::zero();
    }

    {
        let instruction_chal: Vec<u8> = Instruction::Challenge(Box::new(Challenge {
            height: header_400000.number,
            block_hash: Box::new(hash_header(&header_400000, false)),
            element_index: 0,
            merkle_spine: block_with_proofs.merkle_proofs[0].clone(),
            element_pair: {
                let mut iter = block_with_proofs.elements_512();
                Box::new(ElementPair {
                    e0: iter.next().unwrap(),
                    e1: iter.next().unwrap(),
                })
            },
        }))
            .pack();
        process_instruction(&THIS_PROG_ID, &accounts, &instruction_chal)
            .map_err(TestError::ProgError)?;
    };

    Ok(())
}

fn decoded_header_0() -> Result<BlockHeader, TestError> {
    let expected = BlockHeader {
        parent_hash: H256::from([
            0xf7, 0x79, 0xe5, 0x0b, 0x45, 0xbc, 0x27, 0xe4,
            0xed, 0x23, 0x68, 0x40, 0xe5, 0xdb, 0xcf, 0x7a,
            0xfa, 0xb5, 0x0b, 0xea, 0xf5, 0x53, 0xbe, 0x56,
            0xbf, 0x76, 0xda, 0x97, 0x7e, 0x10, 0xcc, 0x73,
        ]),
        uncles_hash: H256::from([
            0x1d, 0xcc, 0x4d, 0xe8, 0xde, 0xc7, 0x5d, 0x7a,
            0xab, 0x85, 0xb5, 0x67, 0xb6, 0xcc, 0xd4, 0x1a,
            0xd3, 0x12, 0x45, 0x1b, 0x94, 0x8a, 0x74, 0x13,
            0xf0, 0xa1, 0x42, 0xfd, 0x40, 0xd4, 0x93, 0x47,
        ]),
        author: H160::from([
            0x52, 0xbc, 0x44, 0xd5,
            0x37, 0x83, 0x09, 0xee,
            0x2a, 0xbf, 0x15, 0x39,
            0xbf, 0x71, 0xde, 0x1b,
            0x7d, 0x7b, 0xe3, 0xb5,
        ]),
        state_root: H256::from([
            0x14, 0xc9, 0x96, 0xb6, 0x93, 0x4d, 0x79, 0x91,
            0x64, 0x36, 0x69, 0xe1, 0x45, 0xb8, 0x35, 0x5c,
            0x63, 0xaa, 0x02, 0xcb, 0xde, 0x63, 0xd3, 0x90,
            0xfc, 0xf4, 0xe6, 0x18, 0x1d, 0x5e, 0xea, 0x45,
        ]),
        transactions_root: H256::from([
            0x79, 0xb7, 0xe7, 0x9d, 0xc7, 0x39, 0xc3, 0x16,
            0x62, 0xfe, 0x6f, 0x25, 0xf6, 0x5b, 0xf5, 0xa5,
            0xd1, 0x42, 0x99, 0xc7, 0xa7, 0xaa, 0x42, 0xc3,
            0xf7, 0x5b, 0x9f, 0xb0, 0x54, 0x74, 0xf5, 0x4c,
        ]),
        receipts_root: H256::from([
            0xe2, 0x8d, 0xc0, 0x54, 0x18, 0x69, 0x2c, 0xb7,
            0xba, 0xab, 0x7e, 0x7f, 0x85, 0xc1, 0xde, 0xdb,
            0x87, 0x91, 0xc2, 0x75, 0xb7, 0x97, 0xea, 0x3b,
            0x1f, 0xfc, 0xae, 0xc5, 0xef, 0x2a, 0xa2, 0x71,
        ]),
        log_bloom: Bloom::from([
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x10, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ]),
        difficulty: U256::from_str("32343734393538373139303230343433")
            .map_err(|_| TestError::HexError)?,
        number: 8982502,
        gas_limit: U256::from(9812622),
        gas_used: U256::from(53465),
        timestamp: 1574455815,
        extra_data: ExtraData::from_slice(&[
            80, 80, 89, 69, 32, 110, 97, 110, 111, 112, 111, 111, 108, 46, 111, 114, 103,
        ]),
        mix_hash: H256::from([
            0xa3, 0x54, 0x25, 0xf4, 0x43, 0x45, 0x2c, 0xf9,
            0x4b, 0xa4, 0xb6, 0x98, 0xb0, 0x0f, 0xd7, 0xb3,
            0xff, 0x4f, 0xc6, 0x71, 0xde, 0xa3, 0xd5, 0xcc,
            0x2d, 0xcb, 0xed, 0xbc, 0x37, 0x66, 0xf4, 0x5e,
        ]),
        nonce: H64::from([0xaf, 0x7f, 0xec, 0x60, 0x31, 0x06, 0x3a, 0x17]),
    };
    return Ok(expected);
}

const DUMMY_ELEM: AccessedElement = AccessedElement {
    address: 0,
    value: H512::zero(),
};

const DUMMY_ELEMS: AccessedElements = AccessedElements([[DUMMY_ELEM; 4]; 32]);
