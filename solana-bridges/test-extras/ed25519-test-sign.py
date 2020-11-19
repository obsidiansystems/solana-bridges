import sys
import binascii
import ed25519

# examples of inputs: see sign.input
# should produce no output: python sign.py < sign.input

# warning: currently 37 seconds/line on a fast machine

# fields on each input line: sk, pk, m, sm
# each field hex
# each field colon-terminated
# sk includes pk at end
# sm includes m at end


while 1:
  line = sys.stdin.readline()
  print line
  if not line: break
  x = line.split(':')
  sk = binascii.unhexlify(x[0][0:64])
  m = binascii.unhexlify(x[2])
  if hasattr(ed25519, "publickey") or hasattr(ed25519, "signature"):
    pk = ed25519.publickey(sk)
    s = ed25519.signature(m,sk,pk)
  else:
    pk = binascii.unhexlify(x[1])
    s = binascii.unhexlify(x[3][0:128])

  ed25519.checkvalid(binascii.unhexlify(x[3][0:128]), m, pk)
  ed25519.checkvalid(s,m,pk)
  forgedsuccess = 0
  try:
    if len(m) == 0:
      forgedm = "x"
    else:
      forgedmlen = len(m)
      forgedm = ''.join([chr(ord(m[i])+(i==forgedmlen-1)) for i in range(forgedmlen)])
    ed25519.checkvalid(s,forgedm,pk)
    forgedsuccess = 1
  except:
    pass
  assert not forgedsuccess
  assert x[0] == binascii.hexlify(sk + pk)
  assert x[1] == binascii.hexlify(pk)
  assert x[3] == binascii.hexlify(s + m)

if getattr(ed25519, "assertions", None) is not None:
  import datetime
  import json
  assertionsFilename = "ed25519-assertions-{0:%Y%m%dT%H%M%S}.txt".format(datetime.datetime.now())
  with open(assertionsFilename, "w") as extraTests:
    for (k, v) in ed25519.assertions.iteritems():
      extraTests.write(json.dumps(v))
      extraTests.write("\n")
  print assertionsFilename
