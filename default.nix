let
  nixpkgs = import ./dep/nixpkgs {};

  solana = with nixpkgs; rustPlatform.buildRustPackage rec {
    pname = "solana";
    version = "v1.3.9";

    src = fetchFromGitHub {
      owner = "solana-labs";
      repo = pname;
      rev = version;
      sha256 = "0fxn2vdh7wkdh9zkl2y3dbprjs5w6mhnr6vp819537h39i2xw90n";
    };

    cargoSha256 = "0lmvixpkzgbkn9lj2w0c809yg5dgjpf8flmhwkk9jvx6dx9wzyqd";
    verifyCargoDeps = true;

    LIBCLANG_PATH="${llvmPackages.libclang}/lib";
    nativeBuildInputs = [ pkgconfig clang llvm ];
    buildInputs = [ libudev openssl ];
    strictDeps = true;

    # TODO: Either allow the nix build to increase file descriptor limit or patch out failing test
    #
    ## [2020-09-09T18:23:04.195996780Z ERROR solana_ledger::blockstore] Unable to increase the maximum open file descriptor limit to 500000
    ## test test_bench_tps_local_cluster_solana ... FAILED
    checkPhase = null;
  };

in {
  inherit nixpkgs solana;
}
