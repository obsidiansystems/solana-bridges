import hashlib

from ed25519_orig import b
from ed25519_orig import q
from ed25519_orig import l
from ed25519_orig import H

assertions = {}

def F25519(e): return (e+0L) % q
def Scalar(e): return (e+0L) % l
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

@accumassertions(F25519)
def xrecover(y):
  xx = (y*y-1) * inv(d*y*y+1)
  x = expmod(xx,(q+3)/8,q)
  if (x*x - xx) % q != 0: x = (x*I) % q
  if x % 2 != 0: x = q-x
  return x

By = (4 * inv(5)) % q
Bx = xrecover(By)
B = [Bx % q,By % q]

@accumassertions(Point, Point)
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

@accumassertions(Point, Scalar)
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

@accumassertions(Point)
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

@accumassertions(Point)
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


@accumassertions(Bytes(32))
def decodepoint(s):
  assert len(s) == b/8
  dec_s = decodeint(s)
  y = dec_s & ((1 << 255) - 1)
  x = xrecover(y)
  if x & 1 != dec_s >> 255: x = q-x
  P = [x,y]
  if not isoncurve(P): raise Exception("decoding point that is not on curve")
  return P

def checkvalid(s,m,pk):
  if len(s) != b/4: raise Exception("signature length is wrong")
  if len(pk) != b/8: raise Exception("public-key length is wrong")
  R = decodepoint(s[0:b/8])
  A = decodepoint(pk)
  S = s[b/8:b/4]
  assert 0 <= decodeint(S) < l # MALEABILITY RESISTANCE
  h = H(encodepoint(R) + pk + m)
  Ah = scalarmult_reduced(A,decodeint_mod(h, l))
  assert isoncurve(Ah)
  BS = scalarmult_reduced(B,decodeint_mod(S, l))
  assert isoncurve(BS)
  RAh = edwards(R,Ah)
  assert isoncurve(RAh)
  if BS != RAh:
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

