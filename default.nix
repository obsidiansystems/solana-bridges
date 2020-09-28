let
  nixpkgs = import ./dep/nixpkgs { overlays = [overlay]; };

  sources = {
    # Not on nixpkgs: https://github.com/hercules-ci/gitignore.nix/issues/6
    gitignore = nixpkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
      sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
    };

    # Hackage release (0.1.0.0) does not support GHC 8.8
    which = nixpkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "which";
      rev = "a7a86bfa1d05d81de4a12a89315bd383763b98ea";
      sha256 = "1635wh4psqbhybbvgjr9gy6f051sb27zlgfamrqw14cdrqdvk5m8";
    };
  };

  gitignoreSource = (import sources.gitignore {}).gitignoreSource;

  overlay = self: super: {
    haskellPackages = with nixpkgs.haskell.lib;
      super.haskellPackages.override (old: {
        overrides = self: super: with nixpkgs.haskell.lib; {
          solana-bridges = overrideCabal (self.callCabal2nix "solana-bridges" (gitignoreSource ./solana-bridges) {}) (drv: {
            executableSystemDepends = (drv.executableSystemDepends or []) ++ (with nixpkgs; [ go-ethereum solc ]);
          });
          web3 = markUnbroken (doJailbreak (dontCheck super.web3));
          which = self.callCabal2nix "which" sources.which {};
        };
      });
  };

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

  solana-rust-bpf = with nixpkgs; stdenv.mkDerivation {
    name = "solana-rust-bpf";
    src = fetchTarball {
      name = "solana-rust-bpf-linux";
      url = "https://github.com/solana-labs/rust-bpf-builder/releases/download/v0.2.3/solana-rust-bpf-linux.tar.bz2";
      sha256 = "0cbwrjwbvd2dyq4w1gnh8d7yyzywqx2k8f32h03z53fmcwldcj1g";
    };
    nativeBuildInputs = [ autoPatchelfHook openssl stdenv.cc.cc.lib ];
    installPhase = ''
       cp -R $src $out
    '';
  };

  solana-llvm = with nixpkgs; stdenv.mkDerivation {
    name = "solana-llvm";
    src = fetchTarball {
      url = "https://github.com/solana-labs/llvm-builder/releases/download/v0.0.15/solana-llvm-linux.tar.bz2";
      sha256 = "09bfj3jg97d2xh9c036xynff0fpg648vhg4sva0sabi6rpzp2c8r";
    };
    nativeBuildInputs = [ autoPatchelfHook stdenv.cc.cc.lib ];
    installPhase = ''
      cp -R $src $out
    '';
  };

  solc = nixpkgs.solc.overrideAttrs (old: {
    # https://github.com/NixOS/nixpkgs/pull/97730
    checkPhase = null;
  });

  xargo = with nixpkgs; rustPlatform.buildRustPackage rec {
    pname = "xargo";

    version = "v0.3.22";

    src = fetchFromGitHub {
      owner = "japaric";
      repo = pname;
      rev = "b7cec9d3dc3720f0b7964f4b6e3a1878f94e4c07";
      sha256 = "0m1dg7vwmmlpqp20p219gsm7zbnnii6lik6hc2vvfsdmnygf271l";
    };

    cargoSha256 = "0jn9flcw5vqvqqm16vxzywqcz47mgbhdh73l6a5f5nxr4m00yy9i";
    verifyCargoDeps = true;

    # TODO: allow tests to run in debug in nixpkgs
    # error[E0554]: `#![feature]` may not be used on the stable release channel
    #  --> tests/smoke.rs:3:1
    # buildType = "debug";
    checkPhase = null;
    strictDeps = true;
    buildInputs = [ makeWrapper ];

    postInstall = ''
      wrapProgram $out/bin/xargo \
      --set-default RUST_BACKTRACE FULL \
    '';
  };

  rust-bpf-sysroot = with nixpkgs; fetchFromGitHub {
    owner = "solana-labs";
    repo = "rust-bpf-sysroot";
    rev = "b4dc90e3ee8a88f197876bc76149add1de7fec25"; # branch v0.12
    sha256 = "1f1w73mkcdld3xxh5vjjc09icafw0z2bskcysay1r0bgbfd5ix82";
  };

  example-helloworld = with nixpkgs; fetchFromGitHub {
    owner = "solana-labs";
    repo = "example-helloworld";
    rev = "0510fcc777a3a4cbbd37e54d09aa806feb128457";
    sha256 = "0zmy74mlxb1mmfzg2zv5w1fni8rb27ddn3rw8sgzzq8gv65hf8qh";
  };

  # TODO build these properly
  spl = with nixpkgs; {
    token = fetchurl {
      url = "https://github.com/solana-labs/solana-program-library/releases/download/token-v2.0.3/spl_token.so";
      sha256 = "0qnkyapd033nbnqsm1hcyrr47pb6kpk9dz88i6j2wqbwhgbqxvp5";
    };
    memo = fetchurl {
      url = "https://github.com/solana-labs/solana-program-library/releases/download/memo-v1.0.0/spl_memo.so";
      sha256 = "0fy664ciriinnk0x6kvsa2wr48prnnrcvlg8g06jpc62kkapn2cv";
    };
  };

  helloWorld = with nixpkgs; stdenv.mkDerivation {
    name = "helloWorld";
    src = example-helloworld;
    buildInputs = [ solana-rust-bpf xargo which rustup ];
    phases = "buildPhase";

    RUST_BACKTRACE="1";
    XARGO_RUST_SRC="/home/alexfmpe/repos/solana/rust-bpf-sysroot/src/";
    RUST_COMPILER_RT_ROOT="/home/alexfmpe/repos/solana/rust-bpf-sysroot/src/compiler-rt";
    RUSTUP_HOME="/home/alexfmpe/rustrustrust";
    RUSTUP_TOOLCHAIN="bpf";

    buildPhase = ''
      source $stdenv/setup
#      export RUSTUP_HOME=$PWD/.rustup-home
#      export RUSTUP_TOOLCHAIN=bpf
#      export XARGO_RUST_SRC=$PWD/rust-bpf-sysroot/src
#      export RUST_COMPILER_RT_ROOT=$PWD/rust-bpf-sysroot/src/compiler-rt
#      export RUST_BACKTRACE=1
      echo 11
      cp -r ${rust-bpf-sysroot} rust-bpf-sysroot
      cp -r ${solana-rust-bpf} solana-rust-bpf
      chmod -R 777 rust-bpf-sysroot
      chmod -R 777 solana-rust-bpf
      ls -l rust-bpf-sysroot/
      ls -l solana-rust-bpf/
      echo 22
      cp -r $src example-helloworld
      echo 33
      which rustc
      echo $USER
      whoami
      echo 44
#      ls -l rust-bpf-sysroot/
#      echo 44
#      chmod 444 rust-bpf-sysroot
      echo 55
      rustup toolchain list -v
      rustup toolchain link bpf $(rustc --print sysroot)
      rustup toolchain list -v
      echo 66
      cd example-helloworld/src/program-rust
      echo 77
#     cargo check
#     cargo build
#     xargo check
#     xargo-check
      xargo build --target bpfel-unknown-unknown --release --no-default-features --features program
    '';
  };

  shell = nixpkgs.haskellPackages.shellFor {
    withHoogle = false; # https://github.com/NixOS/nixpkgs/issues/82245
    packages = p: [ p.solana-bridges ];
    nativeBuildInputs = [ solana-rust-bpf solc ] ++ (with nixpkgs; [ cabal-install ghcid hlint go-ethereum solana xargo rustup ]);

    RUST_BACKTRACE="1";
    XARGO_HOME="/home/alexfmpe/xargoxargoxargo";
    XARGO_RUST_SRC="/home/alexfmpe/repos/solana/rust-bpf-sysroot/src/"; # can't be read-only ???
    RUST_COMPILER_RT_ROOT="${rust-bpf-sysroot}/src/compiler-rt";
    RUSTUP_HOME="/home/alexfmpe/rustrustrust";
    RUSTUP_TOOLCHAIN="bpf";

    SPL_TOKEN=spl.token;
    SPL_MEMO=spl.memo;

    CC="${solana-llvm}/bin/clang"; # has no effect here
    AR="${solana-llvm}/bin/llvm-ar"; # has no effect here

    SOLANA_LLVM_CC="${solana-llvm}/bin/clang"; # has no effect here
    SOLANA_LLVM_AR="${solana-llvm}/bin/llvm-ar"; # has no effect here

    # Get bpf.ld from npm?
    RUSTFLAGS="
      -C lto=no \
      -C opt-level=2 \
      -C link-arg=-z -C link-arg=notext \
      -C link-arg=-T${rust-bpf-sysroot}/bpf.ld \
      -C link-arg=--Bdynamic \
      -C link-arg=-shared \
      -C link-arg=--entry=entrypoint \
      -C link-arg=-no-threads \
      -C linker=${solana-llvm}/bin/ld.lld";
  };

  # export CC=$(nix-build /home/alexfmpe/repos/solana/solana-bridges -A solana-llvm)/bin/clang
  # export AR=$(nix-build /home/alexfmpe/repos/solana/solana-bridges -A solana-llvm)/bin/llvm-ar
  # rustup toolchain link bpf $(rustc --print sysroot)
  # xargo build --target bpfel-unknown-unknown --release --no-default-features --features program

in {
  inherit nixpkgs shell solc solana-rust-bpf solana-llvm helloWorld spl;
  inherit (nixpkgs.haskellPackages) solana-bridges;
}
