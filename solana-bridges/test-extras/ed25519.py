if __import__("os").environ.get("ed25519_py") == "optimised":
    from ed25519_optimised import *
else:
    from ed25519_orig import *
