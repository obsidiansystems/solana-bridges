use quickcheck_macros::quickcheck;

use crate::{
    types::*,
    instruction::*,
    processor::*,
    parameters::*,
};

use std::rc::Rc;
use std::cell::RefCell;

use solana_sdk::{
    account_info::{AccountInfo},
    pubkey::Pubkey,
    program_error::ProgramError,
};

use crate::eth::*;
use crate::parameters::MIN_BUF_SIZE;
use crate::prove::*;
use solana_sdk::clock::Epoch;
use std::str::FromStr;
use std::ops::Deref;
use rlp::{Decodable, Rlp, DecoderError, RlpStream};
use ethereum_types::{U256, H64, H160, H256, Bloom};

mod blocks;
mod relayer_runs;
use blocks::*;

mod inclusion;

// Required to support info! in tests
#[cfg(not(target_arch = "bpf"))]
solana_sdk::program_stubs!();

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
    let block_400000 = Block { header: header_400000, transactions: Vec::new() };
    assert_eq!(block_400000.transactions.len(), 0);
    Ok(())
}

#[quickcheck]
fn test_instructions(mut buf_len: usize, mut block_count: usize) -> Result<(), TestError> {
    buf_len *= std::mem::size_of::<RingItem>() / 7;
    buf_len += MIN_BUF_SIZE;

    block_count += 1;

    let program_id = Pubkey::default();

    let mut raw_data = vec![0; buf_len];

    with_account(&mut *raw_data, |account| {

        let accounts = vec![account];

        let instruction_noop: Vec<u8> = Instruction::Noop.pack();
        process_instruction(&program_id, &accounts, &instruction_noop).map_err(TestError::ProgError)?;

        {
            let header_400000: BlockHeader = decode_rlp(HEADER_400000)?;
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                header: Box::new(header_400000),
                total_difficulty: Box::new(U256([0,1,1,1])) // arbitrarily chosen number for now
            })).pack();
            process_instruction(&program_id, &accounts, &instruction_init).map_err(TestError::ProgError)?;
        }

        for n in 1..block_count {
            {
                let r = accounts[0].data.try_borrow_mut().unwrap();
                let data = interp(&*r);
                println!("ring size: {}, full: {}, current block short no: {}",
                         data.headers.len(), data.full, n);
            }
            let header_4000xx = decode_rlp(HEADER_4000XX[n])?;
            let instruction_new: Vec<u8> = Instruction::NewBlock(header_4000xx).pack();
            process_instruction(&program_id, &accounts, &instruction_new).map_err(TestError::ProgError)?;
        }

        let raw_data = accounts[0].try_borrow_data()
            .map_err(TestError::ProgError)?;
        let data = interp(&*raw_data);

        assert_eq!(block_count % data.headers.len(), data.offset);
        assert_eq!(400000 - 1 + block_count as u64, data.height);
        assert_eq!(block_count >= data.headers.len(), data.full);

        Ok(())
    })
}

// Slow tests ~ 1min each without cache sharing
#[ignore]
#[test]
fn test_pow() -> Result<(), TestError> {
    fn test_header_pow(header: &[u8]) -> Result<bool, TestError> {
        Ok(verify_pow(&decode_rlp(header)?))
    }

    let mut header_400000: BlockHeader = decode_rlp(HEADER_400000)?;
    assert!(verify_pow(&header_400000));
    header_400000.nonce = H64::zero();
    assert!(!verify_pow(&header_400000));
    assert!(test_header_pow(HEADER_400001)?);
    assert!(test_header_pow(HEADER_8996776)?);
    return Ok (());
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

#[test]
fn test_decoding() -> Result<(), TestError> {
    let expected = decoded_header_0()?;
    let header: BlockHeader = decode_rlp(TEST_HEADER_0)?;
    assert_eq!(header, expected);

    let header_400k: BlockHeader = decode_rlp(HEADER_400000)?;
    assert_eq!(header_400k.number, 400000);
    assert_eq!(header_400k.difficulty, U256::from(6022643743806 as u64));
    assert_eq!(hash_header(&header_400k, false), H256::from_str("5d15649e25d8f3e2c0374946078539d200710afc977cdfc6a977bd23f20fa8e8").map_err(|_| TestError::HexError)?);

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

fn decode_rlp <T:Decodable> (bytes: &[u8]) -> Result<T, TestError> {
    let rlp = Rlp::new(bytes);
    return T::decode(&rlp).map_err(TestError::RlpError);
}

pub fn test_inclusion(receipt_index: u64,
                      receipt_data: &[u8],
                      header_data: &[u8],
                      proof_data: &[&[&[u8]]],
) -> Result<(), DecoderError> {
    let header: BlockHeader = rlp::decode(header_data).unwrap();

    let proof_vecs: Vec<_> = proof_data.iter().map(|&node| {
        let mut stream = RlpStream::new();
        stream.append_list::<&[u8], _>(node);
        stream.out()
    }).collect();

    assert!(verify_trie_proof(
        header.receipts_root,
        &*rlp::encode(&receipt_index),
        proof_vecs.iter().map(Deref::deref).map(Ok),
        receipt_data,
    )?);
    Ok(())
}

pub fn pack_proof(proof_data: &[&[&[u8]]]) -> Vec<u8> {
    let proof_vecs: Vec<_> = proof_data.iter().map(|&node| {
        let mut stream = RlpStream::new();
        stream.append_list::<&[u8], _>(node);
        stream.out()
    }).collect();

    let mut stream = RlpStream::new();
    stream.append_list::<Vec<u8>, _>(&*proof_vecs);
    stream.out()
}

pub fn with_account<K, R>(raw_data: &mut [u8], k: K) -> R
where K: FnOnce(AccountInfo) -> R
{
    let key = Pubkey::default();
    let mut lamports = 0;

    let owner = Pubkey::default();

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

#[test]
fn relayer_run() -> Result<(), TestError>
{
    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let program_id = Pubkey::default();
        let accounts = vec![account];

        for instr in &relayer_runs::RUN_0 {
            process_instruction(&program_id, &accounts, instr)
                .unwrap();
        }

        Ok(())
    })
}

pub fn test_inclusion_instruction<F>(
    header_data: &[u8],
    instruction_fun: F,
) -> Result<(), TestError>
where F: FnOnce(BlockHeader) -> ProveInclusion
{
    let mut raw_data = vec![0; 1 << 16];
    with_account(&mut *raw_data, |account| {
        let program_id = Pubkey::default();
        let mut accounts = vec![account];

        let header: BlockHeader = rlp::decode(header_data).unwrap();

        {
            let instruction_init: Vec<u8> = Instruction::Initialize(Box::new(Initialize {
                total_difficulty: Box::new(U256::zero()),
                header: Box::new(header.clone()),
            })).pack();
            process_instruction(&program_id, &accounts, &instruction_init).unwrap();
        }

        {

            accounts[0].is_writable = false;

            let instruction_proove_incl: Vec<u8> = Instruction::ProveInclusion(Box::new(instruction_fun(header))).pack();

            process_instruction(&program_id, &accounts, &instruction_proove_incl)
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
    let res = test_inclusion_instruction(
        HEADER_DATA,
        |header| {
            let mut block_hash = Box::new(hash_header(&header, false));
            block_hash.0[5] +=1;
            ProveInclusion {
                height: header.number,
                block_hash,
                expected_value: RECEIPT_DATA.to_vec(),
                key: rlp::encode(&RECEIPT_INDEX),
                proof: pack_proof(PROOF_DATA),
                min_difficulty: Box::new(U256::zero()),
            }
        },
    );
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_BadBlockHash.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_too_easy() {
    use inclusion::test_0::*;
    // note the err() to require an error;
    let res = test_inclusion_instruction(
        HEADER_DATA,
        |header: BlockHeader| ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256([9,9,9,9])),
        },
    );
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_TooEasy.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_bad_proof() {
    use inclusion::test_0::*;
    // note the err() to require an error;
    let res = test_inclusion_instruction(
        HEADER_DATA,
        |header: BlockHeader| {
            let mut proof = pack_proof(PROOF_DATA);
            proof[5] +=1;
            ProveInclusion {
                height: header.number,
                block_hash: Box::new(hash_header(&header, false)),
                expected_value: RECEIPT_DATA.to_vec(),
                key: rlp::encode(&RECEIPT_INDEX),
                proof,
                min_difficulty: Box::new(U256::zero()),
            }
        },
    );
    assert_eq!(
        res.err().unwrap(),
        TestError::ProgError(CustomError::InvalidProof_BadMerkle.to_program_error()),
    );
}

#[test]
pub fn test_inclusion_instruction_0() -> Result<(), TestError> {
    use inclusion::test_0::*;
    test_inclusion_instruction(
        HEADER_DATA,
        |header: BlockHeader| ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256::zero()),
        },
    )
}

#[test]
pub fn test_inclusion_instruction_1() -> Result<(), TestError> {
    use inclusion::test_1::*;
    test_inclusion_instruction(
        HEADER_DATA,
        |header: BlockHeader| ProveInclusion {
            height: header.number,
            block_hash: Box::new(hash_header(&header, false)),
            expected_value: RECEIPT_DATA.to_vec(),
            key: rlp::encode(&RECEIPT_INDEX),
            proof: pack_proof(PROOF_DATA),
            min_difficulty: Box::new(U256::zero()),
        },
    )
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
        difficulty: U256::from_str("32343734393538373139303230343433").map_err(|_| TestError::HexError)?,
        number: 8982502,
        gas_limit: U256::from(9812622),
        gas_used: U256::from(53465),
        timestamp: 1574455815,
        extra_data: ExtraData { bytes: Vec::from([80, 80, 89, 69, 32, 110, 97, 110, 111, 112, 111, 111, 108, 46, 111, 114, 103]) },
        mix_hash: H256::from([
            0xa3, 0x54, 0x25, 0xf4, 0x43, 0x45, 0x2c, 0xf9,
            0x4b, 0xa4, 0xb6, 0x98, 0xb0, 0x0f, 0xd7, 0xb3,
            0xff, 0x4f, 0xc6, 0x71, 0xde, 0xa3, 0xd5, 0xcc,
            0x2d, 0xcb, 0xed, 0xbc, 0x37, 0x66, 0xf4, 0x5e,
        ]),
        nonce: H64::from([
            0xaf, 0x7f, 0xec, 0x60, 0x31, 0x06, 0x3a, 0x17,
        ]),
    };
    return Ok(expected);
}
