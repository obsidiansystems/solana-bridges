pragma solidity >=0.6.0 <0.8.0;
pragma experimental ABIEncoderV2;

contract SolanaClient {

    function rightrotate(uint64 x, uint64 y) internal pure returns (uint64) {
        assert(y<64);
        return ( (x>>y) | uint64(x<<(64 - y)));
    }

    function Sha512_hash(bytes memory message) internal pure returns (bytes memory) {
        uint64[8] memory H = [
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
        uint num_chunks = ((message.length + 16) >> 7) + 1;
        uint messageLength = message.length;
        for (uint chunk = 0; chunk < num_chunks; ++chunk) {

            for ( uint i = 0 ; i < 16 ; ++i ) { // for each uint64 within the chunk

                uint offset = ((chunk * (1024/8)) + (i * (64/8))); // start of first byte of the word we're looking at.
                uint word = 0;

                if (offset > messageLength) { // if we're past the message
                    if (i == 15) {
                        if ((chunk + 1) == num_chunks) { // last word of the padded message should be the length
                            word = messageLength << 3; // length in bits
                        }
                    }
                } else {
                    assembly {
                        word := shr(192, mload(add(add(offset, 32), message)))
                    }

                    if ((offset + 7) < messageLength) { // all 8 bytes fully contained by message
                    } else {
                        word &= (uint256(~0) << (((offset + 8) - messageLength) * 8)); // zero out garbage bytes.
                        word |= (0x80 << (((offset + 7) - messageLength) * 8)); // set first bit after message to 1
                    }
                }

                w[i] = uint64(word);
            }

            // Extend the first 16 words into the remaining 64 words w[16..79] of the message schedule array:
            for (uint i = 16; i <= 79 ; ++i) {
                uint64 s0 = rightrotate(w[i-15], 1) ^ rightrotate(w[i-15], 8) ^ (w[i-15] >> 7);
                uint64 s1 = rightrotate(w[i-2], 19) ^ rightrotate(w[i-2], 61) ^ (w[i-2] >> 6);
                w[i] = w[i-16] + s0 + w[i-7] + s1;
            }

            // Initialize working variables to current hash value:
            uint64 a = H[0];
            uint64 b = H[1];
            uint64 c = H[2];
            uint64 d = H[3];
            uint64 e = H[4];
            uint64 f = H[5];
            uint64 g = H[6];
            uint64 h = H[7];

            // Compression function main loop:
            for (uint i = 0; i <= 79 ; ++i) {
                uint64 temp1 = h
                    + (rightrotate(e, 14) ^ rightrotate(e, 18) ^ rightrotate(e, 41)) // S1
                    + ((e & f) ^ ((~e) & g)) // ch
                    + k[i] + w[i];
                uint64 temp2 = (rightrotate(a, 28) ^ rightrotate(a, 34) ^ rightrotate(a, 39)) + ((a & b) ^ (a & c) ^ (b & c));

                h = g;
                g = f;
                f = e;
                e = d + temp1;
                d = c;
                c = b;
                b = a;
                a = temp1 + temp2;
            }

            // Add the compressed chunk to the current hash value:
            H[0] = H[0] + a;
            H[1] = H[1] + b;
            H[2] = H[2] + c;
            H[3] = H[3] + d;
            H[4] = H[4] + e;
            H[5] = H[5] + f;
            H[6] = H[6] + g;
            H[7] = H[7] + h;

        }
        uint256[2] memory result =
            [ uint256(H[0]) << 192 | uint256(H[1]) << 128 | uint256(H[2]) << 64 | uint256(H[3])
            , uint256(H[4]) << 192 | uint256(H[5]) << 128 | uint256(H[6]) << 64 | uint256(H[7])
            ];

        return abi.encodePacked(result);
    }

    uint256 constant q = 2**255 - 19;
    uint256 constant EDWARDS_BASEPOINT_ORDER = 2**252 + 27742317777372353535851937790883648493;

    uint256 constant EDWARDS_D = 37095705934669439343138083508754565189542113879843219016388785533085940283555; // -121665 * inv(121666)
    uint256 constant EDWARDS_D2 = EDWARDS_D*2 % q;
    uint256 constant I = 19681161376707505956807079304988542015446066515923890162744021073123829784752; // expmod(2,(q-1)/4,q);

    /// Field % q
    /// type Fq = uint256

    function Fq_add(uint256 self, uint256 other) private pure returns (uint256) {
        return addmod(self, other, q);
    }

    function Fq_sub(uint256 self, uint256 other) private pure returns (uint256) {
        return addmod(self, q - other, q);
    }

    function Fq_mul(uint256 self, uint256 other) private pure returns (uint256) {
        return mulmod(self, other, q);
    }

    function Fq_neg(uint256 self) private pure returns (uint256) {
        return self != 0
            ? q - self
            : self;
    }

    function Fq_pow2k(uint256 s, uint k) private pure returns (uint256) {
        while (k > 0) {
            s = mulmod(s, s, q);
            k = k - 1;
        }
        return s;
    }

    function Fq_square(uint256 self) private pure returns (uint256) {
        return mulmod(self, self, q);
    }

    function Fq_square2(uint256 self) private pure returns (uint256) {
      uint256 self_self = mulmod(self, self, q);
      return Fq_add(self_self, self_self);
    }

    function Fq_is_negative(uint256 self) private pure returns (bool) {
      return self & 1 != 0;
    }

    function Fq_invert(uint256 self) private pure returns (uint256) {
        uint256 t19; uint256 t3;
        (t19, t3) = Fq_pow22501(self);
        uint256 t20 = Fq_pow2k(t19, 5);
        uint256 t21 = Fq_mul(t20, t3);
        return t21;
    }

    function Fq_sqrt_ratio_i(uint256 u, uint256 v) private pure returns (bool, uint256) {

        uint256 v3 = Fq_mul(Fq_square(v), v);
        uint256 v7 = Fq_mul(Fq_square(v3), v);
        uint256 r = Fq_mul(Fq_mul(u, v3), Fq_pow_p58(Fq_mul(u, v7)));
        uint256 check = Fq_mul(v, Fq_square(r));

        bool correct_sign_sqrt   = check ==               u    ;
        bool flipped_sign_sqrt   = check ==        Fq_neg(u)   ;
        bool flipped_sign_sqrt_i = check == Fq_mul(Fq_neg(u),I);

        uint256 r_prime = Fq_mul(I, r);
        r = (flipped_sign_sqrt || flipped_sign_sqrt_i)
            ? r_prime
            : r;

        // Choose the nonnegative square root.
        bool r_is_negative = Fq_is_negative(r);
        r = r_is_negative
            ? Fq_neg(r)
            : r;

        bool was_nonzero_square = correct_sign_sqrt || flipped_sign_sqrt;

        return (was_nonzero_square, r);
    }

    function Fq_pow22501(uint256 self) private pure returns (uint256, uint256) {
        uint256 t0  = Fq_square(self);
        uint256 t2  = Fq_mul(self, Fq_square(Fq_square(t0)));
        uint256 t3  = Fq_mul(t0, t2);
        uint256 t5  = Fq_mul(t2,  Fq_square(t3));
        uint256 t7  = Fq_mul(Fq_pow2k(t5, 5),  t5);
        uint256 t9  = Fq_mul(Fq_pow2k(t7, 10),  t7);
        uint256 t13 = Fq_mul(Fq_pow2k(Fq_mul( Fq_pow2k(t9, 20),  t9), 10),  t7);
        uint256 t15 = Fq_mul(Fq_pow2k(t13, 50),  t13);
        uint256 t19 = Fq_mul(Fq_pow2k(Fq_mul( Fq_pow2k(t15, 100),  t15), 50), t13);

        return (t19, t3);
    }

    function Fq_pow_p58(uint256 self) private pure returns (uint256) {
        uint256 t19;
        uint256 _t3;
        (t19, _t3) = Fq_pow22501(self);
        uint256 t20 = Fq_pow2k(t19, 2);
        uint256 t21 = Fq_mul(self, t20);

        return t21;
    }

    /// Scalar field % l (EDWARDS_BASEPOINT_ORDER)
    /// type Scalar = uint256

    function Scalar_from_bytes(bytes32 s) private pure returns (uint256) {
        uint256 ss = decodeint(s);
        if (ss >= EDWARDS_BASEPOINT_ORDER) {
            revert("scalar not reduced mod l");
        }
        return ss;
    }

    function Scalar_from_bytes_mod_order(bytes memory s) private pure returns (uint256) {
        uint256 ss = 0;
        for (int i = int(s.length) - 1; i >= 0 ; --i) {
            ss = uint256(uint8(s[uint(i)])) + mulmod(ss, 0x100, EDWARDS_BASEPOINT_ORDER);
        }
        ss = ss % EDWARDS_BASEPOINT_ORDER;
        return ss;
    }

    function Scalar_non_adjacent_form(uint256 self, uint w) private pure returns (int8[256] memory naf) {
        assert (2 <= w && w <= 8);

        uint width = 1 << w;
        uint window_mask = width - 1;

        uint pos = 0;
        uint carry = 0;
        while (pos < 256) {
            // Construct a buffer of bits of the scalar, starting at bit `pos`
            uint256 bit_buf = self >> pos;

            // Add the carry into the current window
            uint window = carry + (bit_buf & window_mask);

            if (window & 1 == 0) {
                pos += 1;
                continue;
            }

            if (window < width/2 ) {
                carry = 0;
                naf[pos] = int8(window);
            } else {
                carry = 1;
                naf[pos] = int8(window - width); // (window as i8).wrapping_sub(width as i8);
            }

            pos += w;
        }
        return naf;
    }

    struct ProjectivePoint { uint256 X; uint256 Y; uint256 Z; }
    struct CompletedPoint { uint256 X; uint256 Y; uint256 Z; uint256 T; }
    struct ProjectiveNielsPoint { uint256 Y_plus_X; uint256 Y_minus_X; uint256 Z; uint256 T2d; }

    /// type NafLookupTable5 = ProjectiveNielsPoint[8]
    function NafLookupTable5_select(ProjectiveNielsPoint[8] memory self, uint8 x) private pure returns (ProjectiveNielsPoint memory) {
        return self[x / 2];
    }

    /// EdwardsPoint
    struct EdwardsPoint { uint256 X; uint256 Y; uint256 Z; uint256 T; }
    function EdwardsPoint_identity() private pure returns (EdwardsPoint memory) {
        return EdwardsPoint( 0, 1, 1, 0);
    }

    function EdwardsPoint_is_small_order(EdwardsPoint memory self) private pure returns (bool) {
        CompletedPoint memory x = ProjectivePoint_double(EdwardsPoint_to_projective(self));
        x = ProjectivePoint_double(CompletedPoint_to_projective(x));
        x = ProjectivePoint_double(CompletedPoint_to_projective(x));
        return EdwardsPoint_eq(CompletedPoint_to_extended(x), EdwardsPoint_identity());
    }

    function EdwardsPoint_add_projective_niels_point(EdwardsPoint memory self, ProjectiveNielsPoint memory other) private pure returns (CompletedPoint memory) {
        uint256 Y_plus_X  = Fq_add(self.Y, self.X);
        uint256 Y_minus_X = Fq_sub(self.Y, self.X);
        uint256 PP = Fq_mul(Y_plus_X , other.Y_plus_X);
        uint256 MM = Fq_mul(Y_minus_X, other.Y_minus_X);
        uint256 TT2d = Fq_mul(self.T, other.T2d);
        uint256 ZZ   = Fq_mul(self.Z, other.Z);
        uint256 ZZ2  = Fq_add(ZZ, ZZ);

        return CompletedPoint(
            Fq_sub(PP, MM),
            Fq_add(PP, MM),
            Fq_add(ZZ2, TT2d),
            Fq_sub(ZZ2, TT2d));
    }

    function EdwardsPoint_sub_projective_niels_point(EdwardsPoint memory self, ProjectiveNielsPoint memory other) private pure returns (CompletedPoint memory) {
        uint256 Y_plus_X  = Fq_add(self.Y, self.X);
        uint256 Y_minus_X = Fq_sub(self.Y, self.X);
        uint256 PM = Fq_mul(Y_plus_X, other.Y_minus_X);
        uint256 MP = Fq_mul(Y_minus_X, other.Y_plus_X);
        uint256 TT2d = Fq_mul(self.T, other.T2d);
        uint256 ZZ   = Fq_mul(self.Z, other.Z);
        uint256 ZZ2  = Fq_add(ZZ, ZZ);

        return CompletedPoint(
            Fq_sub(PM, MP),
            Fq_add(PM, MP),
            Fq_sub(ZZ2, TT2d),
            Fq_add(ZZ2, TT2d));
    }

    function EdwardsPoint_neg(EdwardsPoint memory self) private pure returns (EdwardsPoint memory) {
      return EdwardsPoint(
          Fq_neg(self.X),
                 self.Y,
                 self.Z,
          Fq_neg(self.T));
    }


    function EdwardsPoint_eq(EdwardsPoint memory self, EdwardsPoint memory other) private pure returns (bool) {
      return (Fq_mul(self.X, other.Z) == Fq_mul(other.X, self.Z)
           && Fq_mul(self.Y, other.Z) == Fq_mul(other.Y, self.Z));
    }

    function EdwardsPoint_double(EdwardsPoint memory self) private pure returns (EdwardsPoint memory) {
        return CompletedPoint_to_extended(ProjectivePoint_double(EdwardsPoint_to_projective(self)));
    }

    function EdwardsPoint_compress(EdwardsPoint memory self) internal pure returns (bytes32) {
        uint256 recip = Fq_invert(self.Z);
        uint256 x = Fq_mul(self.X, recip);
        uint256 y = Fq_mul(self.Y, recip);

        bytes32 s = encodeint(y ^ (Fq_is_negative(x) ? 1 << 255 : 0));
        return s;
    }

    function EdwardsPoint_decompress(bytes32 bBytes) private pure returns (EdwardsPoint memory) {
        uint256 Y = decodeint(bBytes) & ((1<<255) - 1);
        uint256 Z = 1;
        uint256 YY = Fq_square(Y);
        uint256 u = Fq_sub(YY, Z);
        uint256 v = Fq_add(Fq_mul(YY, EDWARDS_D), Z);
        uint256 X; bool is_valid_y_coord;
        (is_valid_y_coord, X) = Fq_sqrt_ratio_i(u, v);

        if (!is_valid_y_coord) {
            revert("not on curve");
        }
        bool compressed_sign_bit = bBytes[31] >> 7 != 0;
        X = compressed_sign_bit ? Fq_neg(X) : X;
        return EdwardsPoint(X, Y, Z, Fq_mul(X, Y));
    }

    function EdwardsPoint_to_projective(EdwardsPoint memory self) private pure returns (ProjectivePoint memory) {
        return ProjectivePoint(self.X, self.Y, self.Z);
    }

    function EdwardsPoint_to_projective_niels(EdwardsPoint memory self) private pure returns (ProjectiveNielsPoint memory) {
        return ProjectiveNielsPoint(
            Fq_add(self.Y, self.X),
            Fq_sub(self.Y, self.X),
            self.Z,
            Fq_mul(self.T, EDWARDS_D2));
    }

    function EdwardsPoint_to_projective_niels_table(EdwardsPoint memory A) private pure returns (ProjectiveNielsPoint[8] memory Ai) {
        Ai[0] = EdwardsPoint_to_projective_niels(A);
        EdwardsPoint memory A2 = EdwardsPoint_double(A);
        for (uint i = 0; i < 7; ++i) {
            Ai[i + 1] = EdwardsPoint_to_projective_niels(CompletedPoint_to_extended(EdwardsPoint_add_projective_niels_point(A2, Ai[i])));
        }
    }

    function EdwardsPoint_mul(EdwardsPoint memory A, uint256 a) private pure returns (EdwardsPoint memory) {
        int8[256] memory a_naf = Scalar_non_adjacent_form(a, 5);

        // Find starting index
        uint i = 255;
        for (int j = 255; j >= 0; --j) {
            i = uint(j);
            if (a_naf[i] != 0) {
                break;
            }
        }

        ProjectiveNielsPoint[8] memory table_A = EdwardsPoint_to_projective_niels_table(A);

        ProjectivePoint memory r = ProjectivePoint_identity();
        for(;;) {
            CompletedPoint memory t = ProjectivePoint_double(r);

            if (a_naf[i] > 0) {
                t = EdwardsPoint_add_projective_niels_point(CompletedPoint_to_extended(t), NafLookupTable5_select(table_A, uint8( a_naf[i])));
            } else if (a_naf[i] < 0) {
                t = EdwardsPoint_sub_projective_niels_point(CompletedPoint_to_extended(t), NafLookupTable5_select(table_A, uint8(-a_naf[i])));
            }

            r = CompletedPoint_to_projective(t);

            if (i == 0) {
                break;
            }
            i -= 1;

        }
        return ProjectivePoint_to_extended(r);
    }

    /// ProjectivePoint

    function ProjectivePoint_identity() private pure returns (ProjectivePoint memory) {
        return ProjectivePoint(0, 1, 1);
    }

    function ProjectivePoint_double(ProjectivePoint memory self) private pure returns (CompletedPoint memory) {
        uint256 XX          = Fq_square(self.X);
        uint256 YY          = Fq_square(self.Y);
        uint256 ZZ2         = Fq_square2(self.Z);
        uint256 X_plus_Y    = Fq_add(self.X, self.Y);
        uint256 X_plus_Y_sq = Fq_square(X_plus_Y);
        uint256 YY_plus_XX  = Fq_add(YY, XX);
        uint256 YY_minus_XX = Fq_sub(YY, XX);

        return CompletedPoint(
            Fq_sub(X_plus_Y_sq, YY_plus_XX),
            YY_plus_XX,
            YY_minus_XX,
            Fq_sub(ZZ2, YY_minus_XX));
    }

    function ProjectivePoint_to_extended(ProjectivePoint memory self) private pure returns (EdwardsPoint memory) {
        return EdwardsPoint(
            Fq_mul(self.X, self.Z),
            Fq_mul(self.Y, self.Z),
            Fq_square(self.Z),
            Fq_mul(self.X, self.Y));
    }

    /// CompletedPoint
    function CompletedPoint_to_projective(CompletedPoint memory self) private pure returns (ProjectivePoint memory) {
        return ProjectivePoint(
            Fq_mul(self.X, self.T),
            Fq_mul(self.Y, self.Z),
            Fq_mul(self.Z, self.T));
    }

    function CompletedPoint_to_extended(CompletedPoint memory self) private pure returns (EdwardsPoint memory) {
        return EdwardsPoint (
            Fq_mul(self.X, self.T),
            Fq_mul(self.Y, self.Z),
            Fq_mul(self.Z, self.T),
            Fq_mul(self.X, self.Y));
    }

    /// ProjectiveNielsPoint
    function ProjectiveNielsPoint_identity() private pure returns (ProjectiveNielsPoint memory) {
        return ProjectiveNielsPoint(1, 1, 1, 0);
    }

    function ProjectiveNielsPoint_neg(ProjectiveNielsPoint memory self) private pure returns (ProjectiveNielsPoint memory) {
        return ProjectiveNielsPoint(
            self.Y_minus_X,
            self.Y_plus_X,
            self.Z,
            -(self.T2d));
    }

    function ED25519_BASEPOINT_POINT() private pure returns (EdwardsPoint memory) {
        return EdwardsPoint(
            15112221349535400772501151409588531511454012693041857206046113283949847762202,
            46316835694926478169428394003475163141307993866256225615783033603165251855960,
            1,
            46827403850823179245072216630277197565144205554125654976674165829533817101731);
    }

    function swap_bytes32(bytes32 x) private pure returns (bytes32) {
        uint256 y;
        for (uint i = 0; i < 32; ++i) {
            y = y | (uint256(uint8(x[i])) << (i * 8));
        }
        return bytes32(y);
    }

    function encodeint(uint256 x) internal pure returns (bytes32) {
        return swap_bytes32(bytes32(x));
    }

    function decodeint(bytes32 x) private pure returns (uint256) {
        return uint256(swap_bytes32(x));
    }

    function packMessage(bytes32 encodeR, bytes32 pk, bytes memory message) private pure returns (bytes memory packedMessage) {
        packedMessage = abi.encodePacked(encodeR, pk, message);
    }

    struct Signature {
        EdwardsPoint R;
        uint256 S; // Scalar | EDWARDS_BASEPOINT_ORDER
    }

    function ed25519_valid(bytes memory signature, bytes memory message, bytes32 pk) public pure returns (bool) {
        Signature memory s;
        bytes memory packedMessage;
        EdwardsPoint memory A;
        (s, packedMessage, A) = ed25519_parse(signature, message, pk);
        return ed25519_verify(s, packedMessage, A);
    }

    /// signature to internal form suited to storage.
    function ed25519_parse (bytes memory signature, bytes memory message, bytes32 pk) internal pure returns (Signature memory, bytes memory , EdwardsPoint memory) {
        if (signature.length != 64) { revert("signature length is wrong"); }
        if (pk.length != 32) { revert("public-key length is wrong"); }

        bytes32 s_R_bytes;
        bytes32 S;
        assembly {
            s_R_bytes := mload(add(0x20, signature))
            S := mload(add(0x40, signature))
        }
        EdwardsPoint memory signature_R = EdwardsPoint_decompress(s_R_bytes);

        uint256 signature_S = Scalar_from_bytes(S);
        EdwardsPoint memory A = EdwardsPoint_decompress(pk);
        if (EdwardsPoint_is_small_order(signature_R) || EdwardsPoint_is_small_order(A)) {
            revert("malleable_signature");
        }
        bytes memory packedMessage = packMessage(s_R_bytes, pk, message);
        return (Signature(signature_R, signature_S), packedMessage, A);
    }


    /// perform full signature checks
    function ed25519_verify (Signature memory signature, bytes memory packedMessage, EdwardsPoint memory pk) internal pure returns (bool) {
        bytes memory h = Sha512_hash(packedMessage);
        uint256 k = Scalar_from_bytes_mod_order(h);

        EdwardsPoint memory minus_A = EdwardsPoint_neg(pk);
        EdwardsPoint memory minus_Ak = EdwardsPoint_mul(minus_A, k);
        ProjectiveNielsPoint memory BS = EdwardsPoint_to_projective_niels(EdwardsPoint_mul(ED25519_BASEPOINT_POINT(), signature.S));
        EdwardsPoint memory R = CompletedPoint_to_extended(EdwardsPoint_add_projective_niels_point(minus_Ak, BS));

        return EdwardsPoint_eq(R, signature.R);
    }

    struct Slot {
        bool hasBlock;
        bytes32 blockHash;
        bytes32 leaderPublicKey;
        bytes32 bankHashMerkleRoot;
        uint64 voteCounts;
        bool[] transactionRelayed;
        bytes[] transactionSignatures;
        bytes[] transactionMessages;
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

    function getSlot(uint64 slot) public view returns (Slot memory) {
        return slots[slotOffset(slot)];
    }

    // Workarounds for misbehaving hs-web3 bindings
    function getSlot_(uint64 slot) public view returns (bool, bytes32, bytes32, uint64) {
        Slot storage s = slots[slotOffset(slot)];
        return (s.hasBlock, s.blockHash, s.leaderPublicKey, s.voteCounts);
    }
    function getSignatures(uint64 slot, uint64 transactionIndex) public view returns (bool, bytes memory) {
        Slot memory s = getSlot(slot);
        return (s.transactionRelayed[transactionIndex], s.transactionSignatures[transactionIndex]);
    }
    function getMessage(uint64 slot, uint64 transactionIndex) public view returns (bool, bytes memory) {
        Slot memory s = getSlot(slot);
        return (s.transactionRelayed[transactionIndex], s.transactionMessages[transactionIndex]);
    }


    uint constant challengePayout = 10*1000*1000;
    constructor () payable public {
        creator = msg.sender;
        if(address(this).balance < challengePayout) {
            revert("Deposit too small for challenge payout");
        }
    }

    function initialize(
            uint64 slot,
            bytes32 blockHash,
            bytes32 leader,
            bool scheduleWarmup,
            uint64 scheduleFirstNormalEpoch,
            uint64 scheduleLeaderScheduleSlotOffset,
            uint64 scheduleFirstNormalSlot,
            uint64 scheduleSlotsPerEpoch,
            Transaction[] calldata txs
        ) external {
        if(initialized)
            revert("already initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");

        Slot storage s = slots[slotOffset(slot)];
        s.hasBlock = true;
        s.leaderPublicKey = leader;
        s.blockHash = blockHash;

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

        s.transactionRelayed = new bool[](txs.length);
        s.transactionSignatures = new bytes[](txs.length);
        s.transactionMessages = new bytes[](txs.length);
        for(uint64 i = 0; i < txs.length; i++) {
            s.transactionRelayed[i] = true;
            s.transactionSignatures[i] = txs[i].signatures;
            bytes memory message = txs[i].message;
            s.transactionMessages[i] = message;
            countVotes(rootSlot, message);
        }

        initialized = true;
    }

    function authorize() internal view {
        if(!initialized)
            revert("not initialized");
        if(creator != msg.sender)
            revert("Sender not trusted");
    }

    struct ParentSlot {
        uint64 number;
        bytes32 blockHash;
    }
    struct NewSlot {
        uint64 number;
        bytes32 blockHash;
        bytes32 leader;
    }
    function addBlocks(ParentSlot calldata parentSlot,
                       NewSlot[] calldata newSlots,
                       Transaction[][] calldata slotTxs
                       ) external {
        authorize();

        if(newSlots[0].number <= lastSlot)
            revert("Already seen slot");
        if(parentSlot.number != lastSlot)
            revert("Unexpected parent slot");
        if(parentSlot.blockHash != lastHash)
            revert("Unexpected parent hash");

        uint length = newSlots.length;

        for(uint i = 0; i < length; i++) {
            uint64 s;
            NewSlot memory newSlot = newSlots[i];
            uint64 nextSlot = newSlot.number;

            for(s = lastSlot + 1; s < nextSlot; s++) {
                Slot storage slot = slots[slotOffset(s)];
                if(slot.hasBlock) {
                    slot.hasBlock = false;
                }
            }
            Slot storage slot = slots[slotOffset(s)];
            slot.hasBlock = true;
            slot.leaderPublicKey = newSlot.leader;
            slot.blockHash = newSlot.blockHash;
            //TODO: store bank hash merkle root for use in verifyTransaction function

            slot.transactionRelayed = new bool[](slotTxs[i].length);
            slot.transactionSignatures = new bytes[](slotTxs[i].length);
            slot.transactionMessages = new bytes[](slotTxs[i].length);
            for(uint64 j = 0; j < slotTxs[i].length; j++) {
                bool relayed = slotTxs[i][j].relayed;
                slot.transactionRelayed[j] = relayed;
                if(relayed) {
                    slot.transactionSignatures[j] = slotTxs[i][j].signatures;
                    bytes memory message = slotTxs[i][j].message;
                    slot.transactionMessages[j] = message;
                    countVotes(s, message);
                } else {
                    delete slot.transactionSignatures[j];
                    delete slot.transactionMessages[j];
                }
            }

            lastSlot = s;
        }

        seenBlocks += uint64(length);
        lastHash = newSlots[length-1].blockHash;
    }

    struct Transaction {
        bool relayed;
        bytes signatures;
        bytes message;
    }

    function slotOffset(uint64 s) private pure returns (uint64) {
        return s % HISTORY_SIZE;
    }

    function test_sha512(bytes memory message) public pure returns (bytes memory) {
        return Sha512_hash(message);
    }

    // function challenge_leader_signature(uint64 slot) {
    //     if (slot < self.earliest_slot) {
    //         reject("block missing");
    //     }
    //     if (!ed25519_valid(slots[slot].signature, slots[slot].blockhash, slots[slot].leader)) {
    //         selfdestruct(sender);
    //     }
    // }

    function test_ed25519_verify(bytes memory signature, bytes memory message, bytes32 pk) public pure returns (bool) {
        return ed25519_valid(signature, message, pk);
    }

    struct SolanaMessage {
        uint8 requiredSignatures;
        uint8 readOnlySignatures;
        uint8 readOnlyUnsigned;
        bytes32[] addresses;
        bytes32 recentBlockHash;
        SolanaInstruction[] instructions;
    }

    struct SolanaInstruction {
        uint8 programId;
        bytes accounts;
        bytes data;
    }

    struct SolanaVote {
        uint64[] slots;
        bytes32 hash;
        bool hasTimestamp;
        uint64 timestamp;
    }

    // Workaround for misbehaving hs-web3 bindings
    function parseSolanaMessage_(bytes memory buffer) public pure returns (uint8, uint8, uint8, bytes32[] memory, bytes32) {
        SolanaMessage memory m;
        m = parseSolanaMessage(buffer);
        return (m.requiredSignatures,
                m.readOnlySignatures,
                m.readOnlyUnsigned,
                m.addresses,
                m.recentBlockHash
                );
    }

    function parseSolanaMessage(bytes memory buffer) internal pure returns (SolanaMessage memory) {
        SolanaMessage memory solanaMsg;
        uint cursor;
        solanaMsg.requiredSignatures = uint8(buffer[cursor]); cursor++;
        solanaMsg.readOnlySignatures = uint8(buffer[cursor]); cursor++;
        solanaMsg.readOnlyUnsigned = uint8(buffer[cursor]); cursor++;

        uint length;
        (length, cursor) = parseCompactWord16(buffer, cursor);
        solanaMsg.addresses = new bytes32[](length);
        for(uint i = 0; i < length; i++) {
            (solanaMsg.addresses[i], cursor) = parseBytes32(buffer, cursor);
        }

        (solanaMsg.recentBlockHash, cursor) = parseBytes32(buffer, cursor);

        (length, cursor) = parseCompactWord16(buffer, cursor);
        solanaMsg.instructions = new SolanaInstruction[](length);
        for(uint i = 0; i < length; i++) {
            (solanaMsg.instructions[i], cursor) = parseInstruction(buffer, cursor);
        }

        return solanaMsg;
    }

    // Workaround for misbehaving hs-web3 bindings
    function parseInstruction_(bytes memory buffer, uint cursor) public pure returns (uint8, bytes memory, bytes memory, uint) {
        SolanaInstruction memory instruction;
        (instruction, cursor) = parseInstruction(buffer,cursor);
        return (instruction.programId, instruction.accounts, instruction.data, cursor);
    }

    function parseInstruction(bytes memory buffer, uint cursor) internal pure returns (SolanaInstruction memory, uint) {
        SolanaInstruction memory instruction;
        instruction.programId = uint8(buffer[cursor]); cursor++;
        (instruction.accounts, cursor) = parseBytes(buffer, cursor);
        (instruction.data, cursor) = parseBytes(buffer, cursor);
        return (instruction, cursor);
    }

    function parseUint32LE(bytes memory buffer, uint cursor) internal pure returns (uint32, uint) {
        uint32 u;
        for (uint i = 0; i < 4; i++) {
            u |= uint32(uint256(uint8(buffer[cursor + i])) << (i * 8));
        }
        return (u, cursor + 4);
    }

    function parseUint64LE(bytes memory buffer, uint cursor) internal pure returns (uint64, uint) {
        uint64 u;
        for (uint i = 0; i < 8; i++) {
            u |= uint64(uint256(uint8(buffer[cursor + i])) << (i * 8));
        }
        return (u, cursor + 8);
    }

    function parseBytes32(bytes memory buffer, uint cursor) internal pure returns (bytes32, uint) {
        bytes32 b32;
        for (uint i = 0; i < 32; i++) {
            b32 |= bytes32(buffer[cursor + i]) >> (i * 8);
        }
        return (b32, cursor + 32);
    }

    function parseSignature(bytes memory buffer, uint cursor) internal pure returns (bytes memory, uint) {
        bytes memory b64 = new bytes(64);
        assembly {
            let offset := add(buffer, cursor)
            mstore(add(0x20, b64), mload(add(0x20, offset)))
            mstore(add(0x40, b64), mload(add(0x40, offset)))
        }
        return (b64, cursor + 64);
    }

    function parseBytes(bytes memory buffer, uint cursor) internal pure returns (bytes memory, uint) {
        uint16 bytesLength;
        (bytesLength, cursor) = parseCompactWord16(buffer, cursor);
        bytes memory bs = new bytes(bytesLength);
        for (uint i = 0; i < bytesLength; i++) {
            bs[i] = buffer[cursor + i];
        }
        return (bs, cursor + bytesLength);
    }

    function parseVote(bytes memory buffer, uint cursor) internal pure returns (SolanaVote memory, uint) {
        SolanaVote memory vote;

        uint64 length;
        (length, cursor) = parseUint64LE(buffer, cursor);
        vote.slots = new uint64[](length);
        for(uint i = 0; i < length; i++) {
            (vote.slots[i], cursor) = parseUint64LE(buffer, cursor);
        }

        (vote.hash, cursor) = parseBytes32(buffer, cursor);

        vote.hasTimestamp = buffer[cursor] != 0; cursor++;
        if(vote.hasTimestamp) {
            (vote.timestamp, cursor) = parseUint64LE(buffer, cursor);
        }

        return (vote, cursor);
    }

    function parseCompactWord16(bytes memory bs, uint cursor) internal pure returns (uint16, uint) {
        uint8 b0 = uint8(bs[cursor]); cursor++;
        uint16 w = b0 & 0x7f;
        if (b0 < (1 << 7))
            return (w, cursor);

        uint8 b1 = uint8(bs[cursor]); cursor++;
        w |= uint16(b1 & 0x7f) << 7;
        if (b1 < (1 << 7))
            return (w, cursor);

        uint8 b2 = uint8(bs[cursor]); cursor++;
        w |= uint16(b2 & 0x03) << 14;
        if (b2 < (1 << 2))
            return (w, cursor);

        revert("Invalid Compact-u16");
    }

    uint256 constant voteTag = 2;
    uint256 constant voteSwitchTag = 6;
    bytes32 constant voteProgram = hex"0761481d357474bb7c4d7624ebd3bdb3d8355e73d11043fc0da3538000000000";

    function countVotes(uint64 slot, bytes memory message) internal {
        SolanaMessage memory parsedMessage = parseSolanaMessage(message);
        for(uint i = 0; i < parsedMessage.instructions.length; i++) {
            SolanaInstruction memory instruction = parsedMessage.instructions[i];
            if (parsedMessage.addresses[instruction.programId] != voteProgram) {
                continue;
            }

            // https://docs.rs/solana-vote-program/1.4.13/solana_vote_program/vote_instruction/enum.VoteInstruction.html#variant.Vote
            bytes memory data = instruction.data;
            uint tag;
            uint cursor;
            (tag, cursor) = parseUint32LE(data, 0);
            if (tag != voteTag && tag != voteSwitchTag) {
                continue;
            }

            uint64 length; uint64 votedSlot;
            (length, cursor) = parseUint64LE(data, cursor);
            for(uint j = 0; j < length; j++) {
                (votedSlot, cursor) = parseUint64LE(data, cursor);
                if(votedSlot + 32 < slot || slot <= votedSlot)
                    revert("Voting for invalid slot");
                slots[slotOffset(votedSlot)].voteCounts++;
            }
        }
    }

    function verifyVote(bytes memory signatures, bytes memory message, uint64 instructionIndex) public pure returns (bool) {
        uint signaturesCount; uint signaturesOffset;
        (signaturesCount, signaturesOffset) = parseCompactWord16(signatures, 0);

        SolanaMessage memory parsedMessage = parseSolanaMessage(message);
        SolanaInstruction memory instruction = parsedMessage.instructions[instructionIndex];

        // To prevent the relayer from hiding votes to avoid challenges, we always check one signature
        // This might not be needed once stake supermajority checks are possible
        bytes memory signature; uint cursor;
        (signature, cursor) = parseSignature(signatures, signaturesOffset);
        if(! ed25519_valid(signature, message, parsedMessage.addresses[0])) {
            return false;
        }

        if (parsedMessage.addresses[instruction.programId] != voteProgram) {
            revert("Not a vote instruction");
        }
        // https://docs.rs/solana-vote-program/1.4.13/solana_vote_program/vote_instruction/enum.VoteInstruction.html#variant.Vote
        bytes memory data = instruction.data;
        uint tag;
        (tag, cursor) = parseUint32LE(data, 0);
        if (tag != voteTag && tag != voteSwitchTag) {
            revert("Not a Vote nor VoteSwitch instruction");
        }
        SolanaVote memory vote;
        (vote, cursor) = parseVote(data, cursor);

        uint8 signer = uint8(instruction.accounts[3]);
        bytes32 pk = parsedMessage.addresses[signer];

        (signature, cursor) = parseSignature(signatures, signaturesOffset + signer * 64);

        return ed25519_valid(signature, message, pk);
    }

    function challengeVote(uint64 s, uint64 transactionIndex, uint64 instructionIndex) public {
        Slot storage slot = slots[slotOffset(s)];
        if(!slot.transactionRelayed[transactionIndex]) {
            revert("Transaction was not relayed because it contained no vote instructions");
        }
        bytes storage message = slot.transactionMessages[transactionIndex];
        bytes storage signatures = slot.transactionSignatures[transactionIndex];
        bool valid = verifyVote(signatures, message, instructionIndex);

        if(!valid) {
            selfdestruct(msg.sender);
        }
    }

    function verifyTransaction(bytes32 /* accountsHash */,
                               bytes32 /* blockMerkle */,
                               bytes32[16][] calldata /* subProof */,
                               uint64 /* slot */,
                               bytes calldata /* transaction */,
                               uint64 /* transactionIndex */) external pure returns (bool) {
        revert("Bank hash merkle roots are not available yet");
        /*
        bytes32 bankHashMerkleRoot = slots[slotOffset(slot)].bankHashMerkleRoot;
        return this.verifyTransactionInclusionProof(accountsHash,
                                                    blockMerkle,
                                                    subProof,
                                                    bankHashMerkleRoot,
                                                    transaction,
                                                    transactionIndex);
        */
    }

    function verifyTransactionInclusionProof(bytes32 accountsHash,
                                             bytes32 blockMerkle,
                                             bytes32[16][] memory subProof,
                                             bytes32 bankHashMerkleRoot,
                                             bytes memory transaction,
                                             uint64 transactionIndex) public pure returns (bool) {
        if (! verifyMerkleProof(subProof, blockMerkle, transaction, transactionIndex))
            return false;

        bytes memory hashable = abi.encodePacked(accountsHash, blockMerkle);
        return bankHashMerkleRoot == sha256(hashable);
    }

    function verifyMerkleProof(bytes32[16][] memory proof, bytes32 root, bytes memory value, uint64 index) public pure returns (bool) {
        bytes32 hash = sha256(value);

        // uint64 used for index only fits 16 nibbles
        if(proof.length > 16)
            revert("Proof too large");

        for (uint height = 0; height < proof.length; height++) {
            uint64 offset = index % 16;

            if(hash != proof[height][offset]) {
                return false;
            }

            bytes memory hashable = new bytes(32 * 16);
            for(uint i = 0; i < 16; i++) {
                for(uint j = 0; j < 32; j++) {
                    hashable[i*32+j] = proof[height][i][j];
                }
            }

            index = index / 16;
            hash = sha256(hashable);
        }

        return hash == root;
    }
}
