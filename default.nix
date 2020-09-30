let
  nixpkgs = import ./dep/nixpkgs { overlays = [overlay]; };

  nix-thunk = import ./dep/nix-thunk { pkgs = nixpkgs; };

  # Per thunk notes:
  #
  #  gitignore.nix: Not on nixpkgs: https://github.com/hercules-ci/gitignore.nix/issues/6
  #
  #  which: Hackage release (0.1.0.0) does not support GHC 8.8
  sources = nix-thunk.mapSubdirectories nix-thunk.thunkSource ./dep;

  gitignoreSource = (import sources."gitignore.nix" {}).gitignoreSource;

  overlay = self: super: {
    haskellPackages = with nixpkgs.haskell.lib;
      super.haskellPackages.override (old: {
        overrides = self: super: with nixpkgs.haskell.lib; {
          solana-bridges = overrideCabal (self.callCabal2nix "solana-bridges" (gitignoreSource ./solana-bridges) {}) (drv: {
            executableSystemDepends = (drv.executableSystemDepends or []) ++ (with nixpkgs; [ go-ethereum solana solc ]);
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
    sha256 = "1jiw61bdxb10s2xnf9lcw8aqra35vq2a95kk01kz72kqm63rijy8";
    fetchSubmodules = true;
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

  # TODO: https://github.com/NixOS/nixpkgs/pull/95542/files

  helloWorld = with nixpkgs; stdenv.mkDerivation {
    RUST_BACKTRACE="1";
    RUSTUP_TOOLCHAIN="bpf";
    XARGO_RUST_SRC="${rust-bpf-sysroot}/src";
    RUST_COMPILER_RT_ROOT="${rust-bpf-sysroot}/src/compiler-rt";

    name = "helloWorld";
    src = example-helloworld;
    buildInputs = [ solana-rust-bpf xargo which rustup ];
    phases = "buildPhase";

    buildPhase = ''
      export XARGO_HOME="$PWD/xargoxargoxargo";
      export CARGO_HOME="$PWD/cargo-home";

      # source $stdenv/setup
      export RUSTUP_HOME=$PWD/.rustup-home
      # ls -l solana-rust-bpf/
      cp -r $src example-helloworld
      which rustc
      rustup toolchain link bpf $(rustc --print sysroot)
      cd example-helloworld/src/program-rust
      xargo build --target bpfel-unknown-unknown --release --no-default-features --features program
    '';
  };

  shell = nixpkgs.haskellPackages.shellFor {
    withHoogle = false; # https://github.com/NixOS/nixpkgs/issues/82245
    packages = p: [ p.solana-bridges ];
    nativeBuildInputs = [ solana-rust-bpf solc ] ++ (with nixpkgs;
      [ cabal-install ghcid hlint
        go-ethereum solana
        xargo rustup cargo-deps cargo-watch
        shellcheck ninja cmake
      ]);

    RUST_BACKTRACE="1";
    RUSTUP_TOOLCHAIN="bpf";
    XARGO_RUST_SRC="${rust-bpf-sysroot}/src";
    RUST_COMPILER_RT_ROOT="${rust-bpf-sysroot}/src/compiler-rt";

    SPL_TOKEN=spl.token;
    SPL_MEMO=spl.memo;

    SOLANA_LLVM_CC="${solana-llvm}/bin/clang"; # CC gets overwritten
    SOLANA_LLVM_AR="${solana-llvm}/bin/llvm-ar"; # AR gets overwritten

    CARGO_TARGET_DIR="target-bpf";

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

  shell-x86 = with nixpkgs; mkShell {
    buildInputs = [ rustc cargo cargo-watch  ];
  };

  solana-ethereum-client = nixpkgs.rustPlatform.buildRustPackage {
    name = "solana-ethereum-client";
    src = gitignoreSource ./solana-bridges/solana-ethereum-client;
    #nativeBuildInputs = [ pkgs.openssl pkgs.pkgconfig ];
    #buildInputs = [ rustPackages.rust-std ];
    verifyCargoDeps = true;

    # Cargo hash must be updated when Cargo.lock file changes.
    cargoSha256 = "139zdyd80h2zpihbkx39pcjh2axz2nv2npmingkrph4bkzw1r7j9";
  };

in {
  inherit nixpkgs shell solc solana-rust-bpf solana-llvm spl
    solana-ethereum-client;
  inherit (nixpkgs.haskellPackages) solana-bridges;

  shells = {
    target-x86 = shell-x86;
  };
}
