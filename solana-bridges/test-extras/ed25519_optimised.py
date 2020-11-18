# -*- coding: utf8 -*-
import hashlib

from ed25519_orig import b
from ed25519_orig import q
from ed25519_orig import l
from ed25519_orig import H

from collections import namedtuple

# assertions = {}


class Fq(long):

    def __str__(self): return "%d" % long(self)
    def __repr__(self): return "Fq(%d)" % long(self)
    def __new__(cls, value, *args, **kwargs):
        if not (0 <= value < q):
            value = value % q
        return  super(cls, cls).__new__(cls, value)

    def __add__(self, other): assert False, "Fq.add"
    def __sub__(self, other): assert False, "Fq.sub"
    def __mul__(self, other): assert False, "Fq.mul"
    def __neg__(self): assert False, "Fq.neg"
    def __div__(self, other): assert False, "Fq.div"
        # return Fq.add(self, other)

    @staticmethod
    def add(self, other):
        assert isinstance(self, Fq) and isinstance(other, Fq)
        return self.__class__(addmod(long(self), long(other), q))


    @staticmethod
    def sub(self, other):
        assert isinstance(self, Fq) and isinstance(other, Fq)
        return self.__class__(submod(long(self), long(other), q))

    @staticmethod
    def mul(self, other):
        assert isinstance(self, Fq) and isinstance(other, Fq)
        return self.__class__(mulmod(long(self), long(other), q))

    # @staticmethod
    # def div(self, other):
    #     assert isinstance(self, Fq) and isinstance(other, Fq)
    #     return Fq.mul(self, Fq.invert(other))

    @staticmethod
    def neg(self):
        assert isinstance(self, Fq)
        return self.__class__(q - self) if self != 0 else self

    @staticmethod
    def pow2k(self, k): # return self.__class__(expmod(long(self), 2**k, q))
      assert isinstance(self, Fq)
      s = long(self)
      assert k > 0
      while k > 0:
        s = mulmod(s, s, q)
        k = k - 1
      return Fq(s)

    @staticmethod
    def square(self):
      assert isinstance(self, Fq)
      return Fq.pow2k(self, 1)

    @staticmethod
    def square2(self):
      assert isinstance(self, Fq)
      self_self = Fq.pow2k(self, 1)
      return Fq.add(self_self, self_self)

    @staticmethod
    def is_negative(self):
      assert isinstance(self, Fq)
      return bool(long(self) & 1)

    @staticmethod
    def invert(self):
      assert isinstance(self, Fq)
      (t19, t3) = Fq.pow22501(self)   # t19: 249..0 ; t3: 3,1,0
      t20 = Fq.pow2k(t19, 5)            # 254..5
      t21 = Fq.mul(t20, t3)              # 254..5,3,1,0

      # assert long(t21) == inv(long(self))
      return t21

    @staticmethod
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
        assert isinstance(u, Fq) and isinstance(v, Fq)
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

        v3 = Fq.mul(Fq.square(v), v);
        v7 = Fq.mul(Fq.square(v3), v);
        r = Fq.mul(Fq.mul(u, v3), Fq.pow_p58(Fq.mul(u, v7)));
        check = Fq.mul(v, Fq.square(r));

        i = Fq(I)
        correct_sign_sqrt   = check ==               u    ;
        flipped_sign_sqrt   = check ==        Fq.neg(u)   ;
        flipped_sign_sqrt_i = check == Fq.mul(Fq.neg(u),i);

        r_prime = Fq.mul(i, r);
        r = r_prime if flipped_sign_sqrt or flipped_sign_sqrt_i else r

        # Choose the nonnegative square root.
        r_is_negative = Fq.is_negative(r)
        r = Fq.neg(r) if r_is_negative else r

        was_nonzero_square = correct_sign_sqrt or flipped_sign_sqrt

        return (was_nonzero_square, r)

    @staticmethod
    def pow22501(self):
        """ Compute (self^(2^250-1), self^11), used as a helper function
            within invert() and pow22523().
        """
        assert isinstance(self, Fq)
        # Each temporary variable t_i is of the form (self)^e_i.
        # Squaring t_i corresponds to multiplying e_i by 2,
        # so the pow2k function shifts e_i left by k places.
        # Multiplying t_i and t_j corresponds to adding e_i + e_j.

        t0  = Fq.square(self)           # 1         e_0 = 2^1
        t1  = Fq.square(Fq.square(t0))    # 3         e_1 = 2^3
        t2  = Fq.mul(self, t1)              # 3,0       e_2 = 2^3 + 2^0
        t3  = Fq.mul(t0, t2)               # 3,1,0
        t4  = Fq.square(t3)             # 4,2,1
        t5  = Fq.mul( t2,  t4)               # 4,3,2,1,0
        t6  = Fq.pow2k(t5, 5)             # 9,8,7,6,5
        t7  = Fq.mul( t6,  t5)               # 9,8,7,6,5,4,3,2,1,0
        t8  = Fq.pow2k(t7, 10)            # 19..10
        t9  = Fq.mul( t8,  t7)               # 19..0
        t10 = Fq.pow2k(t9, 20)            # 39..20
        t11 = Fq.mul( t10,  t9)              # 39..0
        t12 = Fq.pow2k(t11, 10)           # 49..10
        t13 = Fq.mul( t12,  t7)              # 49..0
        t14 = Fq.pow2k(t13, 50)           # 99..50
        t15 = Fq.mul( t14,  t13)             # 99..0
        t16 = Fq.pow2k(t15, 100)          # 199..100
        t17 = Fq.mul( t16,  t15)             # 199..0
        t18 = Fq.pow2k(t17, 50)           # 249..50
        t19 = Fq.mul(t18, t13)             # 249..0

        return (t19, t3)

    @staticmethod
    def pow_p58(self):
        """ Raise this field element to the power (p-5)/8 = 2^252 -3. """
        # The bits of (p-5)/8 are 101111.....11.
        #
        #                             nonzero bits of exponent
        assert isinstance(self, Fq)
        (t19, _) = Fq.pow22501(self)    # 249..0
        t20 = Fq.pow2k(t19, 2)            # 251..2
        t21 = Fq.mul(self, t20)             # 251..2,0

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

    def __add__(self, other): assert False, "Scalar.add"
    def __sub__(self, other): assert False, "Scalar.sub"
    def __neg__(self): assert False, "Scalar.neg"
    def __div__(self, other): assert False, "Scalar.div"

    def __mul__(self, other):
        if isinstance(other, Scalar): assert False, "Scalar.mul_scalar"
        elif isinstance(other, EdwardsPoint): assert False, "Scalar.mul_edwards_point"
        else: raise TypeError


    @staticmethod
    def from_bytes(b):
        x = decodeint(b)
        if not (0 <= x < l): raise ValueError(b, x)
        return Scalar(x)

    @staticmethod
    def from_bytes_mod_order(b):
        return Scalar(decodeint_mod(b, l))


    @staticmethod
    def add(self, other):
        assert isinstance(self, Scalar) and isinstance(other, Scalar)
        return self.__class__(addmod(long(self), long(other), l))

    @staticmethod
    def sub(self, other):
        assert isinstance(self, Scalar) and isinstance(other, Scalar)
        return self.__class__(submod(long(self), long(other), l))

    @staticmethod
    def mul_scalar(self, other):
        assert isinstance(self, Scalar) and isinstance(other, Scalar)
        return self.__class__(mulmod(long(self), long(other), l))

    # @staticmethod
    # def mul_edwards_point(self, other):
    #     assert isinstance(self, Scalar) and isinstance(other, EdwardsPoint)
    #     return other * self

    # @staticmethod
    # def neg(self):
    #     assert isinstance(self, Scalar)
    #     return Scalar(l - self) if self != 0 else self

    @staticmethod
    def non_adjacent_form(self, w):
        assert isinstance(self, Scalar)
        assert 2 <= w <= 8
        naf = [0] * 256

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
    @staticmethod
    def select(self, x):
        assert isinstance(self, NafLookupTable5)
        assert x > 0
        assert x & 1 == 1
        assert x < 16
        return self[x / 2]


class EdwardsPoint(namedtuple("EdwardsPoint", "X Y Z T")):
    @staticmethod
    def identity():
        return EdwardsPoint(
            X=Fq(0),
            Y=Fq(1),
            Z=Fq(1),
            T=Fq(0))

    @staticmethod
    def is_small_order(self):
        x2 = ProjectivePoint.double(EdwardsPoint.to_projective(self))
        x4 = ProjectivePoint.double(CompletedPoint.to_projective(x2))
        x8 = ProjectivePoint.double(CompletedPoint.to_projective(x4))
        return EdwardsPoint.eq(CompletedPoint.to_extended(x8), EdwardsPoint.identity())

    def __add__(self, other):
        if isinstance(other, ProjectiveNielsPoint): assert False, "EdwardsPoint.add_projective_niels_point"
        elif isinstance(other, EdwardsPoint): assert False, "EdwardsPoint.add"
        else: raise TypeError(other)

    @staticmethod
    def add_projective_niels_point(self, other):
        assert isinstance(self, EdwardsPoint) and isinstance(other, ProjectiveNielsPoint)
        Y_plus_X  = Fq.add(self.Y, self.X);
        Y_minus_X = Fq.sub(self.Y, self.X);
        PP = Fq.mul(Y_plus_X , other.Y_plus_X);
        MM = Fq.mul(Y_minus_X, other.Y_minus_X);
        TT2d = Fq.mul(self.T, other.T2d);
        ZZ   = Fq.mul(self.Z, other.Z);
        ZZ2  = Fq.add(ZZ, ZZ);

        return CompletedPoint(
            X= Fq.sub(PP, MM),
            Y= Fq.add(PP, MM),
            Z= Fq.add(ZZ2, TT2d),
            T= Fq.sub(ZZ2, TT2d))

    # @staticmethod
    # def add(self, other):
    #     assert isinstance(self, EdwardsPoint) and  isinstance(other, EdwardsPoint)
    #     return (self + other.to_projective_niels()).to_extended()

    def __sub__(self, other):
        if isinstance(other, ProjectiveNielsPoint): assert False, "EdwardsPoint.sub_projective_niels_point"
        if isinstance(other, EdwardsPoint): assert False, "EdwardsPoint.sub"
        else: raise TypeError(other)

    @staticmethod
    def sub_projective_niels_point(self, other):
        assert isinstance(self, EdwardsPoint) and isinstance(other, ProjectiveNielsPoint)
        Y_plus_X  = Fq.add(self.Y, self.X);
        Y_minus_X = Fq.sub(self.Y, self.X);
        PM = Fq.mul(Y_plus_X, other.Y_minus_X);
        MP = Fq.mul(Y_minus_X, other.Y_plus_X);
        TT2d = Fq.mul(self.T, other.T2d);
        ZZ   = Fq.mul(self.Z, other.Z);
        ZZ2  = Fq.add(ZZ, ZZ);

        return CompletedPoint(
            X=Fq.sub(PM, MP),
            Y=Fq.add(PM, MP),
            Z=Fq.sub(ZZ2, TT2d),
            T=Fq.add(ZZ2, TT2d))

    # @staticmethod
    # def sub(self, other):
    #     assert isinstance(self, EdwardsPoint) and isinstance(other, EdwardsPoint)
    #     return (self - other.to_projective_niels()).to_extended()


    def __neg__(self): assert False, "EdwardsPoint.neg"

    @staticmethod
    def neg(self):
      assert isinstance(self, EdwardsPoint)
      return EdwardsPoint(
          X=Fq.neg(self.X),
          Y=       self.Y,
          Z=       self.Z,
          T=Fq.neg(self.T))

    def __eq__(self, other): assert False, "EdwardsPoint.eq"

    @staticmethod
    def eq(self, other):
      assert isinstance(self, EdwardsPoint) and isinstance(other, EdwardsPoint)
      return (Fq.mul(self.X, other.Z) == Fq.mul(other.X, self.Z)
          and Fq.mul(self.Y, other.Z) == Fq.mul(other.Y, self.Z))

    @staticmethod
    def double(self):
        assert isinstance(self, EdwardsPoint)
        return CompletedPoint.to_extended(ProjectivePoint.double(EdwardsPoint.to_projective(self)))

    @staticmethod
    def compress(self):
        assert isinstance(self, EdwardsPoint)
        recip = Fq.invert(self.Z);
        x = Fq.mul(self.X, recip);
        y = Fq.mul(self.Y, recip);

        s = encodeint(long(y) ^ (Fq.is_negative(x) << 255))
        return s

    @staticmethod
    def decompress(b):
        Y = Fq(decodeint(b) & ((1<<255) - 1))
        Z = Fq(1)
        YY = Fq.square(Y)
        u = Fq.sub(YY, Z)
        v = Fq.add(Fq.mul(YY, Fq(d)), Z)
        (is_valid_y_coord, X) = Fq.sqrt_ratio_i(u, v)

        if not is_valid_y_coord:
            raise Exception("not on curve")

        compressed_sign_bit = bool((ord(b[31]) >> 7))
        X = Fq.neg(X) if compressed_sign_bit else X
        return EdwardsPoint(X, Y, Z, T=Fq.mul(X, Y))

    @staticmethod
    def to_projective(self):
        assert isinstance(self, EdwardsPoint)
        return ProjectivePoint(X=self.X, Y=self.Y, Z=self.Z)

    @staticmethod
    def to_projective_niels(self):
        assert isinstance(self, EdwardsPoint)
        return ProjectiveNielsPoint(
            Y_plus_X=  Fq.add(self.Y, self.X),
            Y_minus_X= Fq.sub(self.Y, self.X),
            Z=         self.Z,
            T2d=       Fq.mul(self.T, Fq(EDWARDS_D2)))

    @staticmethod
    def to_projective_niels_table(A):
        assert isinstance(A, EdwardsPoint)
        Ai = [EdwardsPoint.to_projective_niels(A)] * 8
        A2 = EdwardsPoint.double(A)
        for i in range(7):
            Ai[i + 1] = EdwardsPoint.to_projective_niels(CompletedPoint.to_extended(EdwardsPoint.add_projective_niels_point(A2, Ai[i])))
        return NafLookupTable5(Ai)

    def __mul__(A, a): assert False, "EdwardsPoint.mul"

    @staticmethod
    def mul(A, a):
        assert isinstance(A, EdwardsPoint)
        assert isinstance(a, Scalar)
        a_naf = Scalar.non_adjacent_form(a, 5)

        # Find starting index
        i = 255
        for j in range(255,-1,-1):
            i = j
            if a_naf[i] != 0:
                break

        table_A = EdwardsPoint.to_projective_niels_table(A) #  NafLookupTable5::<ProjectiveNielsPoint>::from(A);

        r = ProjectivePoint.identity()
        while True:
            t = ProjectivePoint.double(r)

            if a_naf[i] > 0:
              t = EdwardsPoint.add_projective_niels_point(CompletedPoint.to_extended(t), NafLookupTable5.select(table_A, a_naf[i]))
            elif a_naf[i] < 0:
              t = EdwardsPoint.sub_projective_niels_point(CompletedPoint.to_extended(t), NafLookupTable5.select(table_A, -a_naf[i]))

            r = CompletedPoint.to_projective(t)

            if i == 0:
                break
            i -= 1

        return ProjectivePoint.to_extended(r)

class ProjectivePoint(namedtuple("ProjectivePoint", "X Y Z")):
  def __new__(cls, X, Y, Z):
    assert type(X) is Fq
    assert type(Y) is Fq
    assert type(Z) is Fq
    return super(cls, cls).__new__(cls, X=X, Y=Y, Z=Z)

  @staticmethod
  def identity():
      return ProjectivePoint(
          X=Fq(0),
          Y=Fq(1),
          Z=Fq(1))

  @staticmethod
  def double(self):
      assert isinstance(self, ProjectivePoint)
      XX          = Fq.square(self.X)
      YY          = Fq.square(self.Y)
      ZZ2         = Fq.square2(self.Z)
      X_plus_Y    = Fq.add(self.X, self.Y)
      X_plus_Y_sq = Fq.square(X_plus_Y)
      YY_plus_XX  = Fq.add(YY, XX)
      YY_minus_XX = Fq.sub(YY, XX)

      return CompletedPoint(
          X=Fq.sub(X_plus_Y_sq, YY_plus_XX),
          Y=YY_plus_XX,
          Z=YY_minus_XX,
          T=Fq.sub(ZZ2, YY_minus_XX))

  @staticmethod
  def to_extended(self):
      assert isinstance(self, ProjectivePoint)
      return EdwardsPoint(
          X=Fq.mul(self.X, self.Z),
          Y=Fq.mul(self.Y, self.Z),
          Z=Fq.square(self.Z),
          T=Fq.mul(self.X, self.Y)
          )

class CompletedPoint(namedtuple("CompletedPoint", "X Y Z T")):
    @staticmethod
    def to_projective(self):
        assert isinstance(self, CompletedPoint)
        return ProjectivePoint(
            X=Fq.mul(self.X, self.T),
            Y=Fq.mul(self.Y, self.Z),
            Z=Fq.mul(self.Z, self.T))

    @staticmethod
    def to_extended(self):
        assert isinstance(self, CompletedPoint)
        return EdwardsPoint (
            X= Fq.mul(self.X, self.T),
            Y= Fq.mul(self.Y, self.Z),
            Z= Fq.mul(self.Z, self.T),
            T= Fq.mul(self.X, self.Y))

class ProjectiveNielsPoint(namedtuple("ProjectiveNielsPoint", "Y_plus_X Y_minus_X Z T2d")):
    @staticmethod
    def identity():
        return ProjectiveNielsPoint(
            Y_plus_X=Fq(1),
            Y_minus_X=Fq(1),
            Z=Fq(1),
            T2d=Fq(0))

    def __neg__(self): assert False, "ProjectiveNielsPoint.neg"

    @staticmethod
    def neg(self):
        assert isinstance(self, ProjectiveNielsPoint)
        return ProjectiveNielsPoint(
            Y_plus_X=   self.Y_minus_X,
            Y_minus_X=  self.Y_plus_X,
            Z=          self.Z,
            T2d=        -(self.T2d))



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
# def inv(x): return invmod((x % q), q)

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

# d = mulmod((-121665 % q), inv(121666), q)
d = Fq.mul(Fq.neg(Fq(121665)), Fq.invert(Fq(121666)))
I = expmod(2,(q-1)/4,q)

EDWARDS_D2 = mulmod(2, d, q)


def xrecover(y):
  xx = (y*y-1) * inv(d*y*y+1)
  x = expmod(xx,(q+3)/8,q)
  if (x*x - xx) % q != 0: x = (x*I) % q
  if x % 2 != 0: x = q-x
  return x

By = Fq.mul(Fq(4), Fq.invert(Fq(5))) #  (4 * invmod(5, q)) % q
# Bx = xrecover(By)
# B = [Bx % q,By % q]

def encodeint(y):
  bits = [(y >> i) & 1 for i in range(b)]
  return ''.join([chr(sum([bits[i * 8 + j] << j for j in range(8)])) for i in range(b/8)])

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



ED25519_BASEPOINT_POINT = EdwardsPoint.decompress(encodeint(By))
print "ED25519_BASEPOINT_POINT", ED25519_BASEPOINT_POINT

# @accumassertions(Point, Point)
# def edwards(P, Q):
#   x1 = P[0]
#   y1 = P[1]
#   x2 = Q[0]
#   y2 = Q[1]
# 
#   x1y2 = mulmod(x1, y2, q)
#   x2y1 = mulmod(x2, y1, q)
#   dxy = mulmod(d,mulmod(x2y1,x1y2,q),q)
#   y1y2 = mulmod(y1, y2, q)
#   x1x2 = mulmod(x1, x2, q)
# 
#   return [
#     mulmod(addmod(x1y2, x2y1, q), inv(addmod(1,     dxy , q)), q),
#     mulmod(addmod(y1y2, x1x2, q), inv(addmod(1, neg(dxy), q)), q)]

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

# @accumassertions(Point)
def encodepoint(P):
  x = P[0]
  y = P[1]
  bits = [(y >> i) & 1 for i in range(b - 1)] + [x & 1]
  return ''.join([chr(sum([bits[i * 8 + j] << j for j in range(8)])) for i in range(b/8)])

# def bit(h,i):
#   return (ord(h[i/8]) >> (i%8)) & 1

# def publickey(sk):
#   h = H(sk)
#   a = 2**(b-2) + sum(2**i * bit(h,i) for i in range(3,b-2))
#   A = scalarmult(B,a)
#   return encodepoint(A)

# def Hint(m):
#   h = H(m)
#   return sum(2**i * bit(h,i) for i in range(2*b))

# def signature(m,sk,pk):
#   h = H(sk)
#   a = 2**(b-2) + sum(2**i * bit(h,i) for i in range(3,b-2))
#   r = H(''.join([h[i] for i in range(b/8,b/4)]) + m)
#   R = scalarmult(B,decodeint(r))
#   S = (decodeint(r) + Hint(encodepoint(R) + pk + m) * a) % l
#   return encodepoint(R) + encodeint(S)

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



EIGHT_TORSION = [
    EdwardsPoint(
      X=Fq(0),
      Y=Fq(1),
      Z=Fq(1),
      T=Fq(0)),
    EdwardsPoint(
      X=Fq(14399317868200118260347934320527232580618823971194345261214217575416788799818),
      Y=Fq(55188659117513257062467267217118295137698188065244968500265048394206261417927),
      Z=Fq(1),
      T=Fq(49249569669750663008941572433996126643240416652669157144375776867115012622209)),
    EdwardsPoint(
      X=Fq(38214883241950591754978413199355411911188925816896391856984770930832735035197),
      Y=Fq(0),
      Z=Fq(1),
      T=Fq(0)),
    EdwardsPoint(
      X=Fq(14399317868200118260347934320527232580618823971194345261214217575416788799818),
      Y=Fq(2707385501144840649318225287225658788936804267575313519463743609750303402022),
      Z=Fq(1),
      T=Fq(8646474948907434702843920070347827283394575680151124875353015136841552197740)),
    EdwardsPoint(
      X=Fq(0),
      Y=Fq(57896044618658097711785492504343953926634992332820282019728792003956564819948),
      Z=Fq(1),
      T=Fq(0)),
    EdwardsPoint(
      X=Fq(43496726750457979451437558183816721346016168361625936758514574428539776020131),
      Y=Fq(2707385501144840649318225287225658788936804267575313519463743609750303402022),
      Z=Fq(1),
      T=Fq(49249569669750663008941572433996126643240416652669157144375776867115012622209)),
    EdwardsPoint(
      X=Fq(19681161376707505956807079304988542015446066515923890162744021073123829784752),
      Y=Fq(0),
      Z=Fq(1),
      T=Fq(0)),
    EdwardsPoint(
      X=Fq(43496726750457979451437558183816721346016168361625936758514574428539776020131),
      Y=Fq(55188659117513257062467267217118295137698188065244968500265048394206261417927),
      Z=Fq(1),
      T=Fq(8646474948907434702843920070347827283394575680151124875353015136841552197740))
    ]

assert all(EdwardsPoint.is_small_order(eight_torsion) for eight_torsion in EIGHT_TORSION)
assert not EdwardsPoint.is_small_order(ED25519_BASEPOINT_POINT)


def checkvalid(s,m,pk):
  if len(s) != b/4: raise Exception("signature length is wrong")
  if len(pk) != b/8: raise Exception("public-key length is wrong")

  A = EdwardsPoint.decompress(pk)
  assert pk == EdwardsPoint.compress(A) # XXX

  minus_A = EdwardsPoint.neg(A)
  signature_R = EdwardsPoint.decompress(s[0:b/8])
  if EdwardsPoint.is_small_order(signature_R) or EdwardsPoint.is_small_order(A): raise "malleable_signature"
  h = H(s[0:b/8] + pk + m)
  k = Scalar.from_bytes_mod_order(h)
  signature_S = Scalar.from_bytes(s[b/8:b/4])
  minus_Ak = EdwardsPoint.mul(minus_A, k)
  BS = EdwardsPoint.to_projective_niels(EdwardsPoint.mul(ED25519_BASEPOINT_POINT, signature_S))
  R = CompletedPoint.to_extended(EdwardsPoint.add_projective_niels_point(minus_Ak, BS))

  if not EdwardsPoint.eq(R, signature_R):
    raise Exception("signature does not pass verification")

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

