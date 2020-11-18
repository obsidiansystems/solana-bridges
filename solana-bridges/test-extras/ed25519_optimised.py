# -*- coding: utf8 -*-
import hashlib

from ed25519_orig import b
from ed25519_orig import q
from ed25519_orig import l
from ed25519_orig import H

from collections import namedtuple

# assertions = {}


class FieldElement(long):

    def __new__(cls, value, *args, **kwargs):
        if not (0 <= value < q):
            value = value % q
        return  super(cls, cls).__new__(cls, value)

    def __add__(self, other):
        assert isinstance(other, FieldElement)
        return self.__class__(addmod(long(self), long(other), q))

    def __sub__(self, other):
        assert isinstance(other, FieldElement)
        return self.__class__(submod(long(self), long(other), q))
    def __mul__(self, other):
        assert isinstance(other, FieldElement)
        return self.__class__(mulmod(long(self), long(other), q))

    def __div__(self, other):
        1/0
        assert isinstance(other, FieldElement)
        return self * self.__class__(invmod(long(other), q))

    def __neg__(self): return self.__class__(q - self) if self != 0 else self

    def pow2k(self, k): # return self.__class__(expmod(long(self), 2**k, q))
      s = long(self)
      assert k > 0
      while k > 0:
        s = mulmod(s, s, q)
        k = k - 1
      return FieldElement(s)

    def square(self): return self.pow2k(1)
    def square2(self):
      self_self = self.pow2k(1)
      return self_self + self_self

    def __str__(self): return "%d" % long(self)
    def __repr__(self): return "FieldElement(%d)" % long(self)
    def is_negative(self): return bool(long(self) & 1)

    def invert(self):
        (t19, t3) = self.pow22501()   # t19: 249..0 ; t3: 3,1,0
        t20 = t19.pow2k(5)            # 254..5
        t21 =  t20 *  t3              # 254..5,3,1,0

        assert long(t21) == inv(long(self))
        return t21

    def sqrt_ratio_i(u, v):
        """ Given `FieldElements` `u` and `v`, compute either `sqrt(u/v)`
            or `sqrt(i*u/v)` in constant time.

            This function always returns the nonnegative square root.

            # Return

            - `(Choice(1), +sqrt(u/v))  ` if `v` is nonzero and `u/v` is square;
            - `(Choice(1), zero)        ` if `u` is zero;
            - `(Choice(0), zero)        ` if `v` is zero and `u` is nonzero;
            - `(Choice(0), +sqrt(i*u/v))` if `u/v` is nonsquare (so `i*u/v` is square).
        """
        # Using the same trick as in ed25519 decoding, we merge the
        # inversion, the square root, and the square test as follows.
        #
        # To compute sqrt(α), we can compute β = α^((p+3)/8).
        # Then β^2 = ±α, so multiplying β by sqrt(-1) if necessary
        # gives sqrt(α).
        #
        # To compute 1/sqrt(α), we observe that
        #    1/β = α^(p-1 - (p+3)/8) = α^((7p-11)/8)
        #                            = α^3 * (α^7)^((p-5)/8).
        #
        # We can therefore compute sqrt(u/v) = sqrt(u)/sqrt(v)
        # by first computing
        #    r = u^((p+3)/8) v^(p-1-(p+3)/8)
        #      = u u^((p-5)/8) v^3 (v^7)^((p-5)/8)
        #      = (uv^3) (uv^7)^((p-5)/8).
        #
        # If v is nonzero and u/v is square, then r^2 = ±u/v,
        #                                     so vr^2 = ±u.
        # If vr^2 =  u, then sqrt(u/v) = r.
        # If vr^2 = -u, then sqrt(u/v) = r*sqrt(-1).
        #
        # If v is zero, r is also zero.

        v3 = v.square()  * v;
        v7 = v3.square() * v;
        r = (u * v3) * (u * v7).pow_p58();
        check = v * r.square();

        i = FieldElement(I)
        correct_sign_sqrt   = check == (        u);
        flipped_sign_sqrt   = check == (      (-u));
        flipped_sign_sqrt_i = check == ( ( (-u)*i));

        r_prime = i * r;
        r = r_prime if flipped_sign_sqrt or flipped_sign_sqrt_i else r

        # Choose the nonnegative square root.
        r_is_negative = r.is_negative()
        r = -r if r_is_negative else r

        was_nonzero_square = correct_sign_sqrt or flipped_sign_sqrt

        return (was_nonzero_square, r)

    def pow22501(self):
        """ Compute (self^(2^250-1), self^11), used as a helper function
            within invert() and pow22523().
        """
        # Each temporary variable t_i is of the form (self)^e_i.
        # Squaring t_i corresponds to multiplying e_i by 2,
        # so the pow2k function shifts e_i left by k places.
        # Multiplying t_i and t_j corresponds to adding e_i + e_j.
        #
        # Temporary t_i                      Nonzero bits of e_i
        #
        t0  = self.square()           # 1         e_0 = 2^1
        t1  = t0.square().square()    # 3         e_1 = 2^3
        t2  = self *  t1              # 3,0       e_2 = 2^3 + 2^0
        t3  =  t0 *  t2               # 3,1,0
        t4  = t3.square()             # 4,2,1
        t5  =  t2 *  t4               # 4,3,2,1,0
        t6  = t5.pow2k(5)             # 9,8,7,6,5
        t7  =  t6 *  t5               # 9,8,7,6,5,4,3,2,1,0
        t8  = t7.pow2k(10)            # 19..10
        t9  =  t8 *  t7               # 19..0
        t10 = t9.pow2k(20)            # 39..20
        t11 =  t10 *  t9              # 39..0
        t12 = t11.pow2k(10)           # 49..10
        t13 =  t12 *  t7              # 49..0
        t14 = t13.pow2k(50)           # 99..50
        t15 =  t14 *  t13             # 99..0
        t16 = t15.pow2k(100)          # 199..100
        t17 =  t16 *  t15             # 199..0
        t18 = t17.pow2k(50)           # 249..50
        t19 =  t18 *  t13             # 249..0

        return (t19, t3)

    def pow_p58(self):
        """ Raise this field element to the power (p-5)/8 = 2^252 -3. """
        # The bits of (p-5)/8 are 101111.....11.
        #
        #                             nonzero bits of exponent
        (t19, _) = self.pow22501()    # 249..0
        t20 = t19.pow2k(2)            # 251..2
        t21 = self *  t20             # 251..2,0

        return t21

def i8(value):
    if not (-128 <= value < 128):
        raise ValueError
    return int(value)

class Scalar(long):
    def __new__(cls, value, *args, **kwargs):
        if not (0 <= value < l):
            value = value % l
        return  super(cls, cls).__new__(cls, value)

    @classmethod
    def from_bytes(cls, b):
        x = decodeint(b)
        if not (0 <= x < l): raise ValueError(b, x)
        return cls(x)

    @classmethod
    def from_bytes_mod_order(cls, b):
        return cls(decodeint_mod(b, l))

    def __add__(self, other):
        assert isinstance(other, Scalar)
        return self.__class__(addmod(long(self), long(other), l))

    def __sub__(self, other):
        assert isinstance(other, Scalar)
        return self.__class__(submod(long(self), long(other), l))
    def __mul__(self, other):
        if isinstance(other, Scalar):
            return self.__class__(mulmod(long(self), long(other), l))
        elif isinstance(other, EdwardsPoint):
            return other * self
        else:
            raise TypeError

    def __div__(self, other):
        1/0
    # def __div__(self, other):
    #     assert isinstance(other, Scalar)
    #     return self * self.__class__(invmod(long(other), l))

    def __neg__(self): return self.__class__(l - self) if self != 0 else self

    def non_adjacent_form(self, w):
        assert 2 <= w <= 8
        naf = [0] * 256
        x_u64 = [0] * 5

        width = 1 << w;
        window_mask = width - 1;

        pos = 0;
        carry = 0;
        while pos < 256:
            # Construct a buffer of bits of the scalar, starting at bit `pos`
            bit_buf = self >> pos;

            # Add the carry into the current window
            window = carry + (bit_buf & window_mask);

            if window & 1 == 0:
                # If the window value is even, preserve the carry and continue.
                # Why is the carry preserved?
                # If carry == 0 and window & 1 == 0, then the next carry should be 0
                # If carry == 1 and window & 1 == 0, then bit_buf & 1 == 1 so the next carry should be 1
                pos += 1
                continue;

            if window < width/2:
                carry = 0;
                naf[pos] = i8(window);
            else:
                carry = 1;
                naf[pos] = i8(window - width) # (window as i8).wrapping_sub(width as i8);

            pos += w
        return naf

class NafLookupTable5(list):
    def select(self, x):
        assert x > 0
        assert x & 1 == 1
        assert x < 16
        return self[x / 2]


class EdwardsPoint(namedtuple("EdwardsPoint", "X Y Z T")):
    @classmethod
    def identity(cls):
        return cls(
            X=FieldElement(0),
            Y=FieldElement(1),
            Z=FieldElement(1),
            T=FieldElement(0))

    def __add__(self, other):
        if isinstance(other, ProjectiveNielsPoint):
            Y_plus_X  = self.Y + self.X;
            Y_minus_X = self.Y - self.X;
            PP = Y_plus_X  * other.Y_plus_X;
            MM = Y_minus_X * other.Y_minus_X;
            TT2d = self.T * other.T2d;
            ZZ   = self.Z * other.Z;
            ZZ2  = ZZ + ZZ;

            return CompletedPoint(
                X= PP - MM,
                Y= PP + MM,
                Z= ZZ2 + TT2d,
                T= ZZ2 - TT2d)

        elif isinstance(other, EdwardsPoint):
          return (self + other.to_projective_niels()).to_extended()
        else:
          raise TypeError(other)

    def __sub__(self, other):
        if isinstance(other, ProjectiveNielsPoint):
            Y_plus_X  = self.Y + self.X;
            Y_minus_X = self.Y - self.X;
            PM = Y_plus_X * other.Y_minus_X;
            MP = Y_minus_X  * other.Y_plus_X;
            TT2d = self.T * other.T2d;
            ZZ   = self.Z * other.Z;
            ZZ2  = ZZ + ZZ;

            return CompletedPoint(
                X=PM - MP,
                Y=PM + MP,
                Z=ZZ2 - TT2d,
                T=ZZ2 + TT2d)

        elif isinstance(other, EdwardsPoint):
          return (self - other.to_projective_niels()).to_extended()
        else:
          raise TypeError(other)

    def __neg__(self):
      return EdwardsPoint(
          X=-self.X,
          Y= self.Y,
          Z= self.Z,
          T=-self.T)

    def __eq__(self, other):
      assert type(self) is type(other)
      return (self.X * other.Z == other.X * self.Z
          and self.Y * other.Z == other.Y * self.Z)

    def double(self):
      return self.to_projective().double().to_extended()

    def compress(self):
        recip = self.Z.invert();
        x = self.X * recip;
        y = self.Y * recip;

        s = encodeint(long(y) ^ (x.is_negative() << 255))
        # s[31] ^= x.is_negative().unwrap_u8() << 7;
        return s

    @classmethod
    def decompress(cls, b):
        Y = FieldElement(decodeint(b) & ((1<<255) - 1))
        Z = FieldElement(1)
        YY = Y.square()
        u = YY - Z
        v = YY * FieldElement(d) + Z
        (is_valid_y_coord, X) = FieldElement.sqrt_ratio_i(u, v)

        if not is_valid_y_coord:
            raise Exception("not on curve")

        compressed_sign_bit = bool((ord(b[31]) >> 7))
        X = -X if compressed_sign_bit else X
        return cls(X, Y, Z, T=mulmod(X, Y, q))

    def to_projective(self):
        return ProjectivePoint(X=self.X, Y=self.Y, Z=self.Z)

    def to_projective_niels(self):
        return ProjectiveNielsPoint(
            Y_plus_X=  self.Y + self.X,
            Y_minus_X= self.Y - self.X,
            Z=         self.Z,
            T2d=       self.T * FieldElement(EDWARDS_D2))

    def to_projective_niels_table(A):
        Ai = [A.to_projective_niels()] * 8
        A2 = A.double()
        for i in range(7):
            Ai[i + 1] = (A2 + Ai[i]).to_extended().to_projective_niels()
        return NafLookupTable5(Ai)

    def __mul__(A, a):
        assert isinstance(a, Scalar)
        a_naf = a.non_adjacent_form(5)

        # Find starting index
        i = 255
        # for j in range(255,-1,-1):
        #     i = j
        #     if a_naf[i] != 0:
        #         break

        table_A = A.to_projective_niels_table() #  NafLookupTable5::<ProjectiveNielsPoint>::from(A);

        r = ProjectivePoint.identity()
        while True:
            t = r.double()

            if a_naf[i] > 0:
              t = t.to_extended() + table_A.select( a_naf[i])
            elif a_naf[i] < 0:
              t = t.to_extended() - table_A.select(-a_naf[i])

            r = t.to_projective()

            if i == 0:
                break
            i -= 1

        return r.to_extended()


class ProjectivePoint(namedtuple("ProjectivePoint", "X Y Z")):
  def __new__(cls, X, Y, Z):
    assert type(X) is FieldElement
    assert type(Y) is FieldElement
    assert type(Z) is FieldElement
    return super(cls, cls).__new__(cls, X=X, Y=Y, Z=Z)

  @classmethod
  def identity(cls):
      return cls(
          X=FieldElement(0),
          Y=FieldElement(1),
          Z=FieldElement(1))

  def double(self):
      XX          = self.X.square()
      YY          = self.Y.square()
      ZZ2         = self.Z.square2()
      X_plus_Y    = self.X + self.Y
      X_plus_Y_sq = X_plus_Y.square()
      YY_plus_XX  = YY + XX
      YY_minus_XX = YY - XX

      return CompletedPoint(
          X=X_plus_Y_sq - YY_plus_XX,
          Y=YY_plus_XX,
          Z=YY_minus_XX,
          T=ZZ2 - YY_minus_XX)

  def to_extended(self):
      return EdwardsPoint(
          X=self.X * self.Z,
          Y=self.Y * self.Z,
          Z=self.Z.square(),
          T=self.X * self.Y
          )

class CompletedPoint(namedtuple("CompletedPoint", "X Y Z T")):
    def to_projective(self):
        return ProjectivePoint(
            X= self.X * self.T,
            Y= self.Y * self.Z,
            Z= self.Z * self.T)

    def to_extended(self):
        return EdwardsPoint (
            X= self.X * self.T,
            Y= self.Y * self.Z,
            Z= self.Z * self.T,
            T= self.X * self.Y)



# class AffineNielsPoint(namedtuple("AffineNielsPoint", "y_plus_x y_minus_x xy2d")):
#     @classmethod
#     def identity(cls):
#         return cls(
#             y_plus_x=FieldElement(1),
#             y_minus_x=FieldElement(1),
#             xy2d=FieldElement(0))
# 
#     def __neg__(self):
#         return AffineNielsPoint(
#             y_plus_x=   self.y_minus_x,
#             y_minus_x=  self.y_plus_x,
#             xy2d=       -(self.xy2d))


class ProjectiveNielsPoint(namedtuple("ProjectiveNielsPoint", "Y_plus_X Y_minus_X Z T2d")):
    @classmethod
    def identity(cls):
        return cls(
            Y_plus_X=FieldElement(1),
            Y_minus_X=FieldElement(1),
            Z=FieldElement(1),
            T2d=FieldElement(0))

    def __neg__(self):
        return ProjectiveNielsPoint(
            Y_plus_X=   self.Y_minus_X,
            Y_minus_X=  self.Y_plus_X,
            Z=          self.Z,
            T2d=        -(self.T2d))


def F25519(e): return (e+0L) % q
# def Scalar(e): return (e+0L) % l
def Point(e): return map(F25519, e)
def Bytes(numBytes):
  def _Bytes(x):
    assert isinstance(x, str)
    assert len(x) == numBytes
    return x
  return _Bytes


def accumassertions(*types):
  def wrapper(fn):
    import binascii
    def fixbytes(x):
      if isinstance(x, str): return binascii.hexlify(x)
      else: return x
    def wrapped(*args, **kwargs):
      assert len(types) == len(args)
      tyargs = [ty(arg) for (arg, ty) in zip(args, types)]
      # assert all(arg == tyarg for (arg, tyarg) in zip(args, tyargs))

      key = "{0}{1}".format(fn.func_name,tuple(args))
      assert kwargs == {}
      result = fn(*args)
      # tyresult = fn(*tyargs)
      # assert tyresult == result
      assert result is not None
      old_result = assertions.get(key)
      if old_result is not None:
        assert fixbytes(result) == old_result["result"]
      assertions[key] = {
        "fn": fn.func_name,
        "args": map(fixbytes, tyargs),
        "result": fixbytes(result),
      }
      return result

    return wrapped
  return wrapper

minus_one = q - 1

def mulmod(a, b, m): return (a*b) % m
def iszero(x): return int(not x)
def addmod(a, b, m): return (a+b) % m
def submod(a, b, m):
    assert 0 <= b < m
    return addmod(a, m-b, m)

def neg(a): return q - a

def expmod(b, e, m):
  if (b == 0): return 0
  if (e == 0): return 1
  if (m == 0): raise Exception("non-positive modulus")
  r = 1
  bit = 2 ** 255;
  while True:
    if (bit == 0): return r
    r = mulmod(mulmod(r, r, m), (b ** iszero(iszero((e &  bit     )))), m)
    r = mulmod(mulmod(r, r, m), (b ** iszero(iszero((e & (bit / 2))))), m)
    r = mulmod(mulmod(r, r, m), (b ** iszero(iszero((e & (bit / 4))))), m)
    r = mulmod(mulmod(r, r, m), (b ** iszero(iszero((e & (bit / 8))))), m)
    bit = bit / 16

# def inv(x): return expmod(x,q-2,q)
def inv(x): return invmod((x % q), q)

def invmod(a, p):
  if (a == 0 or a == p or p == 0): raise Exception("inv range")
  if (a > p): a = a % p;
  t1 = 0
  t2 = 1
  r1 = p
  r2 = a
  qq = 0
  while (r2 != 0):
      qq = r1 / r2;
      (t1, t2, r1, r2) = (t2, t1 - int(qq) * t2, r2, r1 - qq * r2);
  if (t1 < 0):
      return (p - (-t1))
  return (t1);

d = mulmod((-121665 % q), inv(121666), q)
I = expmod(2,(q-1)/4,q)

EDWARDS_D2 = mulmod(2, d, q)

# @accumassertions(F25519)
def xrecover(y):
  xx = (y*y-1) * inv(d*y*y+1)
  x = expmod(xx,(q+3)/8,q)
  if (x*x - xx) % q != 0: x = (x*I) % q
  if x % 2 != 0: x = q-x
  return x

By = (4 * inv(5)) % q
Bx = xrecover(By)
B = [Bx % q,By % q]

# @accumassertions(Point, Point)
def edwards(P, Q):
  x1 = P[0]
  y1 = P[1]
  x2 = Q[0]
  y2 = Q[1]

  x1y2 = mulmod(x1, y2, q)
  x2y1 = mulmod(x2, y1, q)
  dxy = mulmod(d,mulmod(x2y1,x1y2,q),q)
  y1y2 = mulmod(y1, y2, q)
  x1x2 = mulmod(x1, x2, q)

  return [
    mulmod(addmod(x1y2, x2y1, q), inv(addmod(1,     dxy , q)), q),
    mulmod(addmod(y1y2, x1x2, q), inv(addmod(1, neg(dxy), q)), q)]

# @accumassertions(Point, Scalar)
def scalarmult(P,e):
  return scalarmult_reduced(P, e % l)

def scalarmult_reduced(P, e):
  assert 0 <= e < l

  Q = [0, 1]
  while e:
    if (e & 1):
      Q = edwards(Q,P)
    e = e >> 1
    P = edwards(P,P)
  return Q

def encodeint(y):
  bits = [(y >> i) & 1 for i in range(b)]
  return ''.join([chr(sum([bits[i * 8 + j] << j for j in range(8)])) for i in range(b/8)])

# @accumassertions(Point)
def encodepoint(P):
  x = P[0]
  y = P[1]
  bits = [(y >> i) & 1 for i in range(b - 1)] + [x & 1]
  return ''.join([chr(sum([bits[i * 8 + j] << j for j in range(8)])) for i in range(b/8)])

def bit(h,i):
  return (ord(h[i/8]) >> (i%8)) & 1

def publickey(sk):
  h = H(sk)
  a = 2**(b-2) + sum(2**i * bit(h,i) for i in range(3,b-2))
  A = scalarmult(B,a)
  return encodepoint(A)

def Hint(m):
  h = H(m)
  return sum(2**i * bit(h,i) for i in range(2*b))

def signature(m,sk,pk):
  h = H(sk)
  a = 2**(b-2) + sum(2**i * bit(h,i) for i in range(3,b-2))
  r = H(''.join([h[i] for i in range(b/8,b/4)]) + m)
  R = scalarmult(B,decodeint(r))
  S = (decodeint(r) + Hint(encodepoint(R) + pk + m) * a) % l
  return encodepoint(R) + encodeint(S)

def curvedistance(P):
  x = P[0]
  y = P[1]

  minux_xx = q - (mulmod(x, x, q));
  yy = mulmod(y, y, q);
  dxxyy = mulmod(d, mulmod(minux_xx, yy, q), q);
  return addmod(minux_xx, addmod(yy, addmod(minus_one, dxxyy, q), q), q);

# @accumassertions(Point)
def isoncurve(P):
  PP = curvedistance(P)
  return PP == 0

def decodeint_mod(s, m):
  ss = 0
  # for e_byte in reversed(s):
  for i in range(len(s) - 1, -1, -1):
    e_byte = s[i]
    ss = ord(e_byte) + mulmod(0x100, ss, m)
  ss = ss % m
  assert ss == decodeint(s) % m
  return ss

def decodeint(s):
  ss = 0
  # for e_byte in reversed(s):
  for i in range(len(s) - 1, -1, -1):
    e_byte = s[i]
    ss = ord(e_byte) + (0x100 * ss)
  return ss


# @accumassertions(Bytes(32))
def decodepoint(s):
  assert len(s) == b/8
  dec_s = decodeint(s)
  y = dec_s & ((1 << 255) - 1)
  x = xrecover(y)
  if x & 1 != dec_s >> 255: x = q-x
  P = [x,y]
  if not isoncurve(P): raise Exception("decoding point that is not on curve")
  asdf = EdwardsPoint.decompress(s)
  asdf_enc = asdf.compress()
  assert asdf_enc == s, locals()
  return P

B_asdf = EdwardsPoint.decompress(encodepoint(B))
assert B == decodepoint(B_asdf.compress())

def dalek_verify_strict(A, message, s):
  if len(s) != b/4: raise Exception("signature length is wrong")
  if len(A) != b/8: raise Exception("public-key length is wrong")
  
  assert A == EdwardsPoint.decompress(A).compress() # XXX

  minus_A = -EdwardsPoint.decompress(A)
  signature_R = EdwardsPoint.decompress(s[0:b/8])
  # if signature_R.is_small_order() or A.is_small_order(): raise malleable_signature
  h = H(s[0:b/8] + A + message)
  k = Scalar.from_bytes_mod_order(h)
  signature_S = Scalar.from_bytes_mod_order(s[b/8:b/4])
  # print "bX", signature_S
  # print "bBX", repr((signature_S * B_asdf).compress())
  R = (k * minus_A + (signature_S * B_asdf).to_projective_niels()).to_extended()

  if not (R == signature_R):
    raise Exception("signature does not pass verification")


def checkvalid(s,m,pk):
  # if len(s) != b/4: raise Exception("signature length is wrong")
  # if len(pk) != b/8: raise Exception("public-key length is wrong")
  # R = decodepoint(s[0:b/8])
  # A = decodepoint(pk)
  # S = s[b/8:b/4]
  # assert 0 <= decodeint(S) < l # MALEABILITY RESISTANCE
  # h = H(encodepoint(R) + pk + m)
  # Ah = scalarmult_reduced(A,decodeint_mod(h, l))
  # assert isoncurve(Ah)
  # # print "b-", decodeint_mod(S, l)
  # BS = scalarmult_reduced(B,decodeint_mod(S, l))
  # # print "bB-", repr(encodepoint(BS))
  # assert isoncurve(BS)
  # RAh = edwards(R,Ah)
  # assert isoncurve(RAh)
  # if BS != RAh:
  #   raise Exception("signature does not pass verification")

  dalek_verify_strict(pk, m, s)

if __name__ == '__main__':
  import json, sys, binascii
  def fixbytes(x):
    if isinstance(x, unicode): return binascii.unhexlify(x)
    else: return x
  for (i, info_raw) in enumerate(sys.stdin):
    info = json.loads(info_raw)
    expected = fixbytes(info["result"])
    actual = globals()[info["fn"]](*(fixbytes(arg) for arg in info["args"]))
    assert actual == expected, (i, info, actual)
    if i % 100 == 0:
      print i, info, actual

