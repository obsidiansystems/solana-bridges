pragma solidity >=0.6.0 <0.8.0;
pragma experimental ABIEncoderV2;

contract SolanaClient {

// library Sha512 {

    function rightrotate(uint64 x, uint64 y) internal pure returns (uint64) {
        assert(y<64);
        return ( (x>>y) | uint64(x<<(64 - y)));
    }

    struct workvars {
        uint64 a;
        uint64 b;
        uint64 c;
        uint64 d;
        uint64 e;
        uint64 f;
        uint64 g;
        uint64 h;
    }

    function Sha512_hash(bytes memory message) internal pure returns (bytes memory) {
        uint64[8] memory h = [
            0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
            0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179];
        uint64[80] memory k = [
            0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc, 0x3956c25bf348b538,
            0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118, 0xd807aa98a3030242, 0x12835b0145706fbe,
            0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2, 0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235,
            0xc19bf174cf692694, 0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
            0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5, 0x983e5152ee66dfab,
            0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4, 0xc6e00bf33da88fc2, 0xd5a79147930aa725,
            0x06ca6351e003826f, 0x142929670a0e6e70, 0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed,
            0x53380d139d95b3df, 0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
            0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30, 0xd192e819d6ef5218,
            0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8, 0x19a4c116b8d2d0c8, 0x1e376c085141ab53,
            0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8, 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373,
            0x682e6ff3d6b2b8a3, 0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
            0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b, 0xca273eceea26619c,
            0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178, 0x06f067aa72176fba, 0x0a637dc5a2c898a6,
            0x113f9804bef90dae, 0x1b710b35131c471b, 0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc,
            0x431d67c49c100d4c, 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817];

        uint64[80] memory w;
        workvars memory v;

        uint num_chunks = ((message.length + 16) >> 7) + 1;
        for (uint chunk = 0; chunk < num_chunks; ++chunk)
        {
            assembly {
                let messageLength := mload(message) // message length

                for { let i := 0 } lt(i, 16) { i := add(i, 1) } { // for each uint64 within the chunk

                    let offset := add(mul(chunk, 128), shl(i, 3)) // start of first byte of the word we're looking at.
                    let word := 0

                    switch gt(offset, messageLength) // if we're past the message
                    case 1 {
                        if eq(i,15) {
                            if eq(add(chunk, 1), num_chunks) { // last word of the padded message should be the length
                                word := messageLength
                            }
                        }
                    }
                    default {
                        word := shr(mload(add(add(0x20, message), offset)), 192) // read a uint64 from message.
                        switch lt(add(offset, 7), messageLength) // all 8 bytes ully contained by message
                        case 1 { }
                        default {
                            word := and(word, shl(sub(0, 1), mul(sub(add(offset, 8), messageLength), 8))) // zero out garbage bytes.
                            word := or(word, shl(0x80, mul(sub(add(offset, 7), messageLength), 8))) // set first bit after message to 1
                        }
                    }

                    mstore(add(w, shl(i, 5)), word)
                }
            }

            // Extend the first 16 words into the remaining 64 words w[16..79] of the message schedule array:
            for (uint i = 16; i <= 79 ; ++i) {
                uint64 s0 = rightrotate(w[i-15], 1) ^ rightrotate(w[i-15], 8) ^ (w[i-15] >> 7);
                uint64 s1 = rightrotate(w[i-2], 19) ^ rightrotate(w[i-2], 61) ^ (w[i-2] >> 6);
                w[i] = w[i-16] + s0 + w[i-7] + s1;
            }

            // Initialize working variables to current hash value:
            v.a = h[0];
            v.b = h[1];
            v.c = h[2];
            v.d = h[3];
            v.e = h[4];
            v.f = h[5];
            v.g = h[6];
            v.h = h[7];

            // Compression function main loop:
            for (uint i = 0; i <= 79 ; ++i) {
                uint64 S0 = rightrotate(v.a, 28) ^ rightrotate(v.a, 34) ^ rightrotate(v.a, 39);
                uint64 S1 = rightrotate(v.e, 14) ^ rightrotate(v.e, 18) ^ rightrotate(v.e, 41);

                uint64 ch = (v.e & v.f) ^ ((~v.e) & v.g);
                uint64 temp1 = v.h + S1 + ch + k[i] + w[i];
                uint64 maj = (v.a & v.b) ^ (v.a & v.c) ^ (v.b & v.c);
                uint64 temp2 = S0 + maj;

                v.h = v.g;
                v.g = v.f;
                v.f = v.e;
                v.e = v.d + temp1;
                v.d = v.c;
                v.c = v.b;
                v.b = v.a;
                v.a = temp1 + temp2;
            }

            // Add the compressed chunk to the current hash value:
            h[0] = h[0] + v.a;
            h[1] = h[1] + v.b;
            h[2] = h[2] + v.c;
            h[3] = h[3] + v.d;
            h[4] = h[4] + v.e;
            h[5] = h[5] + v.f;
            h[6] = h[6] + v.g;
            h[7] = h[7] + v.h;

        }
        uint256[2] memory result =
            [ uint256(h[0]) << 192 | uint256(h[1]) << 128 | uint256(h[2]) << 64 | uint256(h[3])
            , uint256(h[4]) << 192 | uint256(h[5]) << 128 | uint256(h[6]) << 64 | uint256(h[7])
            ];

        return abi.encodePacked(result);
    }


// }
// library ed25519_simple {
    // using Sha512 for Sha512.state;

    uint256 constant b = 256;
    uint256 constant q = 2**255 - 19;
    uint256 constant l = 2**252 + 27742317777372353535851937790883648493;

    uint256 constant d = 37095705934669439343138083508754565189542113879843219016388785533085940283555; // -121665 * inv(121666)
    uint256 constant I = 19681161376707505956807079304988542015446066515923890162744021073123829784752; // expmod(2,(q-1)/4,q);


    uint256 constant By = 46316835694926478169428394003475163141307993866256225615783033603165251855960; // 4 * inv(5)
    uint256 constant Bx = 15112221349535400772501151409588531511454012693041857206046113283949847762202; // xrecover(By)
    // uint256 constant B = [Bx % q,By % q]


    uint256 constant minus_one = q - 1;

    function expmod(uint256 base, uint256 e, uint256 m) internal pure returns (uint256) {
        if (e == 0) {
            return 1;
        } else {
            uint256 t = expmod(base, e >> 1, m);
            t = mulmod(t, t, m);
            if (e & 1 != 0) {
                t = mulmod(t, base, m);
            }
            return t;
        }
    }

    function inv(uint256 x) internal pure returns (uint256) { return expmod(x, q-2, q); }
    function neg(uint256 x) internal pure returns (uint256) { return q - x; }

    function xrecover(uint256 y) internal pure returns (uint256) {
        // xx = (y*y-1) * inv(d*y*y+1)
        uint256 yy = mulmod(y, y, q);
        uint256 xx = mulmod(addmod(yy, minus_one, q), inv(addmod(mulmod(d, yy, q), 1, q)), q);

        uint256 x = expmod(xx, (q + 3 >> 3), q);

        if (mulmod(x, x, q) != xx) { x = mulmod(x, I, q); }
        if (x & 1 != 0) { x = q - x; }

        return x;
    }
    struct edwards_point {
        uint256 x;
        uint256 y;
    }

    function edwards(edwards_point memory P, edwards_point memory Q) internal pure returns (edwards_point memory PQ) {
        uint256 x1 = P.x;
        uint256 y1 = P.y;
        uint256 x2 = Q.x;
        uint256 y2 = Q.y;

        uint256 x1y2 = mulmod(x1, y2, q);
        uint256 x2y1 = mulmod(x2, y1, q);
        uint256 dxy = mulmod(d,mulmod(x2y1,x1y2,q),q);
        uint256 y1y2 = mulmod(y1, y2, q);
        uint256 x1x2 = mulmod(x1, x2, q);


        PQ.x = mulmod(addmod(x1y2, x2y1, q), inv(addmod(1,     dxy , q)), q);
        PQ.y = mulmod(addmod(y1y2, x1x2, q), inv(addmod(1, neg(dxy), q)), q);
    }

    function swap_bytes32(bytes32 x) internal pure returns (bytes32) {
        bytes32 y;
        y  = x[ 0] << (31*8) | x[ 1] << (30*8) | x[ 2] << (29*8) | x[ 3] << (28*8);
        y |= x[ 4] << (27*8) | x[ 5] << (26*8) | x[ 6] << (25*8) | x[ 7] << (24*8);
        y |= x[ 8] << (23*8) | x[ 9] << (22*8) | x[10] << (21*8) | x[11] << (20*8);
        y |= x[12] << (19*8) | x[13] << (18*8) | x[14] << (17*8) | x[15] << (16*8);
        y |= x[16] << (15*8) | x[17] << (14*8) | x[18] << (13*8) | x[19] << (12*8);
        y |= x[20] << (11*8) | x[21] << (10*8) | x[22] << ( 9*8) | x[23] << ( 8*8);
        y |= x[24] << ( 7*8) | x[25] << ( 6*8) | x[26] << ( 5*8) | x[27] << ( 4*8);
        y |= x[28] << ( 3*8) | x[29] << ( 2*8) | x[30] << ( 1*8) | x[31];
        return y;
    }

    function scalarmult(edwards_point memory P, uint256 e) internal pure returns (edwards_point memory Q) {
        if (e == 0) {
            Q.x = 0;
            Q.y = 1;
        } else {
            Q = scalarmult(P,e >> 1);
            Q = edwards(Q,Q);
            if (e & 1 != 0) { Q = edwards(Q,P); }
        }
    }

    function encodeint(uint256 x) internal pure returns (bytes32) {
        return swap_bytes32(bytes32(x));
    }
    function encodepoint(edwards_point memory P) internal pure returns (bytes32) {
        bytes32 y = encodeint(P.y);
        if (P.x & 1 != 0) {
            y |= bytes32(uint256(1) << 255);
        }
        return y;
    }

    uint256 constant e2256 = 38; // 2**256 % q
    function Hint(bytes memory message) internal pure returns (uint256) {
        // bytes memory digest = hh.finalize();
        bytes memory digest = Sha512_hash(message);
        assert(digest.length == 64);
        bytes32 lo_bytes;
        bytes32 hi_bytes;
        assembly {
            lo_bytes := mload(add(0x20, digest))
            hi_bytes := mload(add(0x40, digest))
        }
        uint256 lo = decodeint(lo_bytes);
        uint256 hi = decodeint(hi_bytes);
        return addmod(lo, mulmod(hi, e2256, q), q);
    }

    function isoncurve(edwards_point memory P) internal pure returns (bool) {
        uint256 x = P.x;
        uint256 y = P.y;
        uint256 xx = inv(mulmod(x, x, q));
        uint256 yy = mulmod(y, y, q);
        uint256 dxxyy = mulmod(d, mulmod(xx, yy, q), q);
        return addmod(xx, addmod(yy, addmod(minus_one, dxxyy, q), q), q) == 0;
        // return (-x*x + y*y - 1 - d*x*x*y*y) % q == 0
    }

    function decodeint(bytes32 x) internal pure returns (uint256) {
        return uint256(swap_bytes32(x));
    }

    function decodepoint(bytes32 s) internal pure returns (edwards_point memory P) {
        uint256 dec_s = decodeint(s);
        uint256 y = dec_s & ((1 << 255) - 1);
        uint256 x = xrecover(y);
        if (x & 1 != (dec_s >> 255)) {
            x = q-x;
        }
        P.x = x;
        P.y = y;
        if (!isoncurve(P)) {
            revert("decoding point that is not on curve");
        }
    }

    function checkvalid(bytes memory signature, bytes memory message, bytes32 pk) internal pure {
        if (signature.length != b/4) { revert("signature length is wrong"); }
        if (pk.length != b/8) { revert("public-key length is wrong"); }
        bytes32 s_R_bytes;
        bytes32 s_A_bytes;
        assembly {
            s_R_bytes := mload(add(0x20, signature))
            s_A_bytes := mload(add(0x40, signature))
        }
        edwards_point memory R = decodepoint(s_R_bytes);
        edwards_point memory A = decodepoint(pk);
        uint256 S = decodeint(s_A_bytes);

        uint256 h = Hint(abi.encodePacked(encodepoint(R), pk, message));

        edwards_point memory B;
        B.x = Bx;
        B.y = By;

        edwards_point memory BS = scalarmult(B, S);
        edwards_point memory sigS = edwards(R,scalarmult(A,h));
        if (BS.x != sigS.x || BS.y != sigS.y) {
            revert("signature does not pass verification");
        }
    }

// }

struct Slot {
    bool hasBlock;
    bytes32 blockHash;
    bytes32 leaderPublicKey;
}

    uint64 constant HISTORY_SIZE = 100;
    address immutable public creator;
    bool public initialized;

    // root of trust
    bytes32 public rootHash;
    uint64 public rootSlot;
    bytes32 public rootPublicKey;
    // epoch schedule
    bool warmup;
    uint64 firstNormalEpoch;
    uint64 leaderScheduleSlotOffset;
    uint64 firstNormalSlot;
    uint64 slotsPerEpoch;


    uint64 public seenBlocks;
    bytes32 public lastHash;
    uint64 public lastSlot;
    Slot[HISTORY_SIZE] public slots;

    event Success();

    constructor () public {
        creator = msg.sender;
    }
    function initialize(
            uint64 slot,
            bytes32 blockHash,
            bytes32 leader,
            bool scheduleWarmup,
            uint64 scheduleFirstNormalEpoch,
            uint64 scheduleLeaderScheduleSlotOffset,
            uint64 scheduleFirstNormalSlot,
            uint64 scheduleSlotsPerEpoch
        ) public {
        if(initialized)
            revert("already initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");

        fillSlot(slot, blockHash, leader);
        lastSlot = slot;
        lastHash = blockHash;

        rootHash = blockHash;
        rootSlot = slot;
        rootPublicKey = leader;
        warmup = scheduleWarmup;
        firstNormalEpoch = scheduleFirstNormalEpoch;
        leaderScheduleSlotOffset = scheduleLeaderScheduleSlotOffset;
        firstNormalSlot = scheduleFirstNormalSlot;
        slotsPerEpoch = scheduleSlotsPerEpoch;

        initialized = true;

        emit Success();
    }

    function authorize() internal view {
        if(!initialized)
            revert("not initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");
    }

    function addBlocks(uint64[] calldata blockSlots,
                       bytes32[] calldata blockHashes,
                       uint64[] calldata parentSlots,
                       bytes32[] calldata parentBlockHashes,
                       bytes32[] calldata leaders
                       ) external {
        authorize();
        for(uint i = 0; i < blockSlots.length; i++)
            addBlockAuthorized(blockSlots[i], blockHashes[i], parentSlots[i], parentBlockHashes[i], leaders[i]);
        emit Success();
    }

    function addBlock(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash, bytes32 leaderPublicKey) external {
        authorize();
        addBlockAuthorized(slot, blockHash, parentSlot, parentBlockHash, leaderPublicKey);
        emit Success();
    }

    function addBlockAuthorized(uint64 slot, bytes32 blockHash, uint64 parentSlot, bytes32 parentBlockHash, bytes32 leader) private {
        if(slot <= lastSlot)
            revert("Already seen slot");
        if(parentSlot != lastSlot)
            revert("Unexpected parent slot");
        if(parentBlockHash != lastHash)
            revert("Unexpected parent hash");

        for(uint64 s = lastSlot + 1; s < slot; s++) {
            emptySlot(s);
        }
        fillSlot(slot, blockHash, leader);

        lastSlot = slot;
        lastHash = blockHash;
        seenBlocks++;
    }

    function slotOffset(uint64 s) private pure returns (uint64) {
        return s % HISTORY_SIZE;
    }

    function fillSlot(uint64 s, bytes32 hash, bytes32 leader) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = hash;
        slot.hasBlock = true;
        slot.leaderPublicKey = leader;
    }

    function emptySlot(uint64 s) private {
        Slot storage slot = slots[slotOffset(s)];
        slot.blockHash = 0;
        slot.hasBlock = false;
    }

    // function eq(bytes memory a, bytes memory b) internal pure returns (bool) {
    //     if (a.length != b.length) { return false; }
    //     for (uint i = 0; i < a.length; ++i) {
    //         if (a[i] != b[i]) {
    //             return false;
    //         }
    //     }
    //     return true;
    // }

    function test_sha512(bytes memory message) public pure returns (bytes memory) {
        return Sha512_hash(message);
    }

}
