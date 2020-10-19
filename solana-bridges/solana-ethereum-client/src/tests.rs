use quickcheck_macros::quickcheck;

use crate::{
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
    program_pack::{Pack},
};

use crate::eth::*;
use crate::parameters::MIN_BUF_SIZE;
use crate::prove::*;
use solana_sdk::clock::Epoch;
use std::str::FromStr;
use std::ops::Deref;
use rlp::{Decodable, Encodable, Rlp, DecoderError, RlpStream};
use ethereum_types::{U256, H64, H160, H256, Bloom};
use hex::FromHex;

mod blocks;
use blocks::*;

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
    let header_400000 = decode_rlp(&hex_to_bytes(HEADER_400000)?)?;
    let block_400000 = Block { header: header_400000, transactions: Vec::new() };
    assert_eq!(block_400000.transactions.len(), 0);
    Ok(())
}

#[quickcheck]
fn test_instructions(mut buf_len: usize, mut block_count: usize) -> Result<(), TestError> {
    buf_len *= BlockHeader::LEN / 7;
    buf_len += MIN_BUF_SIZE;

    block_count += 1;

    let program_id = Pubkey::default();
    let key = Pubkey::default();
    let mut lamports = 0;
    let mut raw_data = vec![0; buf_len];

    let owner = Pubkey::default();
    let account = AccountInfo {
        key: &key,
        is_signer: true,
        is_writable: true,
        lamports: Rc::new(RefCell::new(&mut lamports)),
        data: Rc::new(RefCell::new(&mut raw_data)),
        owner: &owner,
        executable: false,
        rent_epoch: Epoch::default(),
    };

    let accounts = vec![account];

    let instruction_noop: Vec<u8> = Instruction::Noop.pack();
    process_instruction(&program_id, &accounts, &instruction_noop).map_err(TestError::ProgError)?;

    {
        let header_400000 = decode_rlp(&hex_to_bytes(HEADER_400000)?)?;
        let instruction_init: Vec<u8> = Instruction::Initialize(header_400000).pack();
        process_instruction(&program_id, &accounts, &instruction_init).map_err(TestError::ProgError)?;
    }

    for n in 1..block_count {
        {
            let r = accounts[0].data.try_borrow_mut().unwrap();
            let data = interp(&*r);
            println!("ring size: {}, current block short no: {}", data.headers.len(), n);
        }
        let header_4000xx = decode_rlp(&hex_to_bytes(HEADER_4000XX[n])?)?;
        let instruction_new: Vec<u8> = Instruction::NewBlock(header_4000xx).pack();
        process_instruction(&program_id, &accounts, &instruction_new).map_err(TestError::ProgError)?;
    }

    let data = interp(&*raw_data);
    assert_eq!(block_count % data.headers.len(), data.offset);
    assert_eq!(400000 - 1 + block_count as u64, data.height);
    assert_eq!(block_count >= data.headers.len(), data.full);
    return Ok(());
}

// Slow tests ~ 1min each without cache sharing
#[ignore]
#[test]
fn test_pow() -> Result<(), TestError> {
    fn test_header_pow(header: &str) -> Result<bool, TestError> {
        Ok(verify_pow(&decode_rlp(&hex_to_bytes(header)?)?))
    }

    let mut header_400000: BlockHeader = decode_rlp(&hex_to_bytes(HEADER_400000)?)?;
    assert!(verify_pow(&header_400000));
    header_400000.nonce = H64::zero();
    assert!(!verify_pow(&header_400000));
    assert!(test_header_pow(HEADER_400001)?);
    assert!(test_header_pow(HEADER_8996776)?);
    return Ok (());
}

fn test_extradata_pack(extra: ExtraData) -> Result<(), TestError> {
    let mut extra_slice = [0; ExtraData::LEN];
    extra.pack_into_slice(&mut extra_slice);
    assert_eq!(extra.bytes.len() as u8, extra_slice[0]);
    assert_eq!(extra, ExtraData::unpack_from_slice(&extra_slice).map_err(TestError::ProgError)?);
    return Ok(());
}

#[test]
fn test_roundtrip_pack() -> Result<(), TestError> {
    test_extradata_pack(ExtraData { bytes: vec![] })?;
    test_extradata_pack(ExtraData { bytes: vec![4] })?;
    test_extradata_pack(ExtraData { bytes: vec![5,5] })?;
    test_extradata_pack(ExtraData { bytes: vec![6,6,6] })?;

    let expected = decoded_header_0()?;
    let mut buffer = [0; BlockHeader::LEN];
    expected.pack_into_slice(&mut buffer);
    let unpacked = BlockHeader::unpack_from_slice(&buffer).map_err(TestError::ProgError)?;
    assert_eq!(expected, unpacked);

    return Ok(());
}

#[test]
fn test_roundtrip_rlp() -> Result<(), TestError> {
    let expected = decoded_header_0()?;
    assert_eq!(expected, decode_rlp(&encode_header(&expected))?);
    return Ok(());
}

#[test]
fn test_decoding() -> Result<(), TestError> {
    let expected = decoded_header_0()?;
    let header: BlockHeader = decode_rlp(&hex_to_bytes(TEST_HEADER_0)?)?;
    assert_eq!(header, expected);

    let header_400k: BlockHeader = decode_rlp(&hex_to_bytes(HEADER_400000)?)?;
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

#[derive(Debug)]
enum TestError {
    HexError,
    RlpError,
    ProgError(ProgramError),
}

fn hex_to_bytes(h: &str) -> Result<Vec<u8>, TestError> {
    return hex::decode(h).map_err(|_| TestError::HexError);
}
fn decode_rlp <T:Decodable> (bytes: &[u8]) -> Result<T, TestError> {
    let rlp = Rlp::new(bytes);
    return T::decode(&rlp).map_err(|_| TestError::RlpError);
}
fn encode_header(header: &BlockHeader) -> Vec<u8> {
    return header.rlp_bytes();
}

pub fn test_inclusion(receipt_index: u64,
                      receipt_data: &[u8],
                      header_data: &[u8],
                      proof_data: &[Vec<Vec<u8>>],
) -> Result<(), DecoderError> {
    let header: BlockHeader = rlp::decode(header_data).unwrap();

    let proof_vecs: Vec<_> = proof_data.iter().map(|node| {
        let mut stream = RlpStream::new();
        stream.begin_list(node.len());
        for item in node {
            stream.append(item);
        }
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

#[test]
pub fn test_inclusion_0() -> Result<(), DecoderError> {
    let receipt_index: u64 = 0;
    let receipt_data = Vec::from_hex("f901a60182d0d9b9010000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000010000000000000000000000000000000000000000000000000000000408000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000010000000000000000000000000000000000000000000000000000000400000000000100000000000000000000000000080000000000000000000000000000000000000000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f89df89b94dac17f958d2ee523a2206206994597c13d831ec7f863a0ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3efa00000000000000000000000006cc5f688a315f3dc28a7781717a9a798a59fda7ba00000000000000000000000007e7a32d9dc98c485c489be8e732f97b4ffe3a4cda000000000000000000000000000000000000000000000000000000001a13b8600").unwrap();
    let header_data = Vec::from_hex("f9021aa0f779e50b45bc27e4ed236840e5dbcf7afab50beaf553be56bf76da977e10cc73a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479452bc44d5378309ee2abf1539bf71de1b7d7be3b5a014c996b6934d7991643669e145b8355c63aa02cbde63d390fcf4e6181d5eea45a079b7e79dc739c31662fe6f25f65bf5a5d14299c7a7aa42c3f75b9fb05474f54ca0e28dc05418692cb7baab7e7f85c1dedb8791c275b797ea3b1ffcaec5ef2aa271b9010000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000010000000000000000000000000000000000000000000000000000000408000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000010000000000000000000000000000000000000000000000000000000400000000000100000000000000000000000000080000000000000000000000000000000000000000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000903234373439353837313930323034343383890fe68395ba8e82d0d9845dd84a079150505945206e616e6f706f6f6c2e6f7267a0a35425f443452cf94ba4b698b00fd7b3ff4fc671dea3d5cc2dcbedbc3766f45e88af7fec6031063a17").unwrap();
    let proof_data: Vec<Vec<Vec<u8>>> = vec![
        vec![
            Vec::from_hex("2080").unwrap(),
            Vec::from_hex("f901a60182d0d9b9010000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000010000000000000000000000000000000000000000000000000000000408000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000010000000000000000000000000000000000000000000000000000000400000000000100000000000000000000000000080000000000000000000000000000000000000000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f89df89b94dac17f958d2ee523a2206206994597c13d831ec7f863a0ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3efa00000000000000000000000006cc5f688a315f3dc28a7781717a9a798a59fda7ba00000000000000000000000007e7a32d9dc98c485c489be8e732f97b4ffe3a4cda000000000000000000000000000000000000000000000000000000001a13b8600").unwrap(),
        ],
    ];

    test_inclusion(receipt_index, &*receipt_data, &*header_data, &*proof_data)
}

#[test]
pub fn test_inclusion_1() -> Result<(), DecoderError> {
    let receipt_index: u64 = 190;
    let header_data = Vec::from_hex("f9021ea059822ac0f17af7578ac8cf2c655a4ccfb1f5622ecad88992a2522e91a3cc401da01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d4934794829bd824b016326a401d083b33d092293333a830a01f54c61cebc3d08954929f359668498e191c1a34adb47f5db2881d414b69631ca0adf860aabb9786198a7d91dec6514ecebfd5aba4565fb7a81f8e68cf819b3000a0dddbc1d4cc2f1f522a3ba1f627d7645945d4922d6eb82c318c7be64735ec0e0db901000416012000c9a88c580040302001880844228042b418e12800bd011510a9580700f15880012010400858c08042dab1408b200403c1484425ac90c800d02c0800050050c04b4cc4b01083211905c820409a000e0c1346200508024c8020c892907120102c0a2840332290050028808c2104611a0c90944a020c002853968810640c0190030c342500584004239a2704228212001dc80004016803a15610920180070c89000a802115094a4082450080522001318488a8098a08c28000188025580490a4aa00b2802012001050868800140d0fd20800d8200682226060119420049032000832001a4011780096081c7654002800100d9484102144ec889094a44a903233363631373837353134373338393883984f928398684283986153845eac64b1947070796520e4b883e5bda9e7a59ee4bb99e9b1bca0814ab4871d625ff894a6736dcf195f4ce1c0f5dd8a3d42a1e9587c2a9e1d522988dd7482ba86e3b14a").unwrap();
    let receipt_data = Vec::from_hex("f901a701839590b6b9010000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000001000000000000000000000000000000010000000000000000000008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000004000000000000000010000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000600000000000f89df89b945885d2a27bd4c6d111b83bc3fc359ed951e8e6f8f863a0ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3efa00000000000000000000000009b24ee0bfbf708b541fb65b6087d6e991a0d11a8a00000000000000000000000005a012de1a4c89b496e0a276158552abe6f843a6fa0000000000000000000000000000000000000000000000001135631d5283a4000").unwrap();

    let proof_data = vec![
        vec![
            Vec::from_hex("c684a63b18c933f0b45d6863ec63d5beb49e191992479fe3cbf1dc6709661337").unwrap(),
            Vec::from_hex("297cb12d9b6b25bff0bd3199bfd2fdb4b3fca45416ecc355958b68dc1b7bc7a3").unwrap(),
            Vec::from_hex("870bde7cc109d51463b1b8e77ebb36cb4021ddc7df0fd31c3140cad3260236e8").unwrap(),
            Vec::from_hex("8ddcf32ce5a1ac912bddc28f2f25df0921aaff8f100f2979028b27baa5b191f1").unwrap(),
            Vec::from_hex("4de7d3b04b549af847ffcc757f8aa121b02bd00c1233e90547fac99da7049e19").unwrap(),
            Vec::from_hex("206564eec53442ea3334438820c75c1f8bdb617430956f07a08e66f5990458ae").unwrap(),
            Vec::from_hex("b11876d993891898b6397d0fa744783ba5bf35e79a329c7b9710e9026c2957c8").unwrap(),
            Vec::from_hex("719cc527cb7fcec3efaf017c5c4801609fa41572befae0468c24ac48fd389437").unwrap(),
            Vec::from_hex("e59bf6fa8f9359d07651b581b3875fc8aea5b85ba0eab9e23503dfa273b9b0f0").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
        ],
        vec![
            Vec::from_hex("a123f503897e190fa46ac478716778ebff6c1661b676c910cb4d4c6764457e06").unwrap(),
            Vec::from_hex("4ca92d89e85b0bc5a8670844967a027f012db12fb027808532c0ad61a751747e").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
        ],
        vec![
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("578357a5e1c44fdccf2cc34d27142b04ec433fe3c8bc57a35c4971f6a82a4da4").unwrap(),
            Vec::from_hex("4d6d3155937f76f01a359ebff4336a504ede8e283e555e1194468883854b7b11").unwrap(),
            Vec::from_hex("ce5478625cca3b625badaa26d231cbd072735b72307b7bce43a7917bd8ba04be").unwrap(),
            Vec::from_hex("66d11ae6215298daa654e04de8649aeb1e116f7138bf3d921c5cbe0a52589116").unwrap(),
            Vec::from_hex("e80733626a805d2974d9b97db52e18d2ce28f532f96552098bf0e656c6bb39a9").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
            Vec::from_hex("").unwrap(),
        ],
        vec![
            Vec::from_hex("46935fcc8ffcc89c8eae03fef89ac9fbde75be14b64c810fb1447d328907456d").unwrap(),
            Vec::from_hex("15ec39ed1284cdf4840d6b7511ff34aed164fd9e4b920b1fdb6fde54958d3a17").unwrap(),
            Vec::from_hex("e02aae4672f8b80f468759b092e4eb809edc8cf4b513df11b21d4233556c0b4d").unwrap(),
            Vec::from_hex("0efe37f3d3919b0bed73152bee545e033eb1fad5ddd8e383747decfd5ae93231").unwrap(),
            Vec::from_hex("16616fa5c192ddf9d0c2e1b1d70dc02a83f490b7d729072ac129d9f58f1e166d").unwrap(),
            Vec::from_hex("edb363ac38c62779bf449b0a51bb0b79a9541e8e5b12a8660185fa729795bedc").unwrap(),
            Vec::from_hex("c59dfcdfefed6a01b761ffd820cec8b8f52d9cb287759441cbd73e2a78ba2416").unwrap(),
            Vec::from_hex("893e6dacc036de93725e0e5112d849a735837a94536635318227b448535c6eb2").unwrap(),
            Vec::from_hex("dd8b4c24d7246d9c3a86fef130a5a3bc53f10d1ab706789d6914b8580c4c0202").unwrap(),
            Vec::from_hex("17c0abe1c74e0d01db47e4e2f2d75555d0ea3a2fc590670186ac23dd31944ec0").unwrap(),
            Vec::from_hex("4535f5fc448300be236c1e7f28576296b449592f5adbac59bf35a40e2f7701be").unwrap(),
            Vec::from_hex("bbf8eca1063220d003071437da87689887491c999913f5aba6ed21915c78407c").unwrap(),
            Vec::from_hex("1875e64c3cb31254ff0cf8041d4053c33e3f6aad8faad2c9323729324eaf1a9e").unwrap(),
            Vec::from_hex("7546cbb2b0137e669738c5ab69fb2071f755b253468f90da48855d41411d2e77").unwrap(),
            Vec::from_hex("d9677df9ddf784f9d20f24737b40cb372478114b225df8af225c0f40aaa843f5").unwrap(),
            Vec::from_hex("5b34e3dcfa33c72c4645793531246a91b74af04d564eebd63d00d0e274880283").unwrap(),
            Vec::from_hex("").unwrap(),
        ],
        vec![
            Vec::from_hex("20").unwrap(),
            Vec::from_hex("f901a701839590b6b9010000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000001000000000000000000000000000000010000000000000000000008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000004000000000000000010000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000600000000000f89df89b945885d2a27bd4c6d111b83bc3fc359ed951e8e6f8f863a0ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3efa00000000000000000000000009b24ee0bfbf708b541fb65b6087d6e991a0d11a8a00000000000000000000000005a012de1a4c89b496e0a276158552abe6f843a6fa0000000000000000000000000000000000000000000000001135631d5283a4000").unwrap(),
        ],
    ];

    test_inclusion(receipt_index, &*receipt_data, &*header_data, &*proof_data)
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
