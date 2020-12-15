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
            executableSystemDepends = (drv.executableSystemDepends or []) ++ [solana solana-client-tool ethashproof] ++ (with nixpkgs; [ go-ethereum solc ]);
          });
          http-client-tls = doJailbreak (super.http-client-tls);
          web3 = doJailbreak (dontCheck (self.callCabal2nix "web3" sources.hs-web3 {}));
          which = self.callCabal2nix "which" sources.which {};
        };
      });
  };

  solana = with nixpkgs; rustPlatform.buildRustPackage rec {
    pname = "solana";
    version = "v1.3.9";

    # TODO: upstream
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = pname;
      rev = "db2f8ec4fc7b9ccbfdc68ace67d767dbac9330dd"; # branch: debug-elf
      sha256 = "0ffih3armr6fdys40dzdc913rkpaxrgyfiw7030kp0nqbarhr0d4";
    };

    cargoSha256 = "1hdphhl6acj48z11ciznisb826yk8njv79ri46yzznybx6bqybrh";
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

  shell = nixpkgs.haskellPackages.shellFor {
    withHoogle = false; # https://github.com/NixOS/nixpkgs/issues/82245
    packages = p: with p; [ solana-bridges ];
    nativeBuildInputs = [ solana-rust-bpf solc ] ++ (with nixpkgs;
      [ cabal-install ghcid hlint
        go-ethereum solana
        xargo cargo-deps cargo-watch rustfmt clippy
        shellcheck ninja cmake
        jq
        solana-client-tool
      ]);

    RUST_BACKTRACE="1";
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

  shells = {
    ethereum-client-bpf = shell;

    ethereum-client-x86 = nixpkgs.mkShell {
      nativeBuildInputs = with nixpkgs.buildPackages; [ rustc cargo cargo-deps cargo-watch clippy rustfmt ];
    };

    solana-client-evm = with nixpkgs; mkShell {
      buildInputs = [ inotify-tools go-ethereum solc ];
    };

    scripts = with nixpkgs; mkShell {
      buildInputs = [ solana solana-client-tool ];
    };
  };

  ethereum-client-src = gitignoreSource ./solana-bridges/ethereum-client;

  # Cargo hash must be updated when Cargo.lock file changes.
  ethereum-client-dep-sha256 = "00xyzzdnm4wkp65bqq04v6arg0zrq1nzxc79xd0yp8449kw2gijv";
  ethereum-client-dep-srcs = nixpkgs.rustPlatform.fetchCargoTarball {
    name = "ethereum-client";
    src = ethereum-client-src;
    sourceRoot = null;
    sha256 = ethereum-client-dep-sha256;
  };

  # TODO: https://github.com/NixOS/nixpkgs/pull/95542/files
  mk-ethereum-client = cargoBuildFlags: nixpkgs.rustPlatform.buildRustPackage {
    name = "ethereum-client";
    src = ethereum-client-src;
    #cargoVendorDir = ethereum-client-dep-srcs;
    #nativeBuildInputs = [ pkgs.openssl pkgs.pkgconfig ];
    #buildInputs = [ rustPackages.rust-std ];
    verifyCargoDeps = true;

    inherit cargoBuildFlags;

    cargoSha256 = ethereum-client-dep-sha256;
  };

  solana-client-tool = (import ./solana-client-tool {pkgs = nixpkgs;}).package;

  ethereum-client-no-prog = mk-ethereum-client [ ];

  ethereum-client-prog = mk-ethereum-client [ "--features" "program" ];

  withSPLEnv = binName: nixpkgs.runCommand binName {
    nativeBuildInputs = [ nixpkgs.makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper "${nixpkgs.haskellPackages.solana-bridges}/bin/${binName}" "$out/bin/${binName}" \
      --set-default SPL_TOKEN "${spl.token}" \
      --set-default SPL_MEMO "${spl.memo}"
  '';

  generate-solana-genesis = withSPLEnv "generate-solana-genesis";

  run-solana-testnet = withSPLEnv "run-solana-testnet";

  ethashproof = with nixpkgs; buildGoModule rec {
    name = "ethashproof";
    runVend = true;

    src = fetchFromGitHub {
      owner = "tranvictor";
      repo = name;
      rev = "82a2b716eac4965709898a3dae791b4bace0999a";
      sha256 = "0xahaiv9i289lp76c0zb68qbz8xk3r2r0grl85zhbk2iykmg6jby";
    };
    vendorSha256 = "0chs20pgcxg2wf7y3ppsqfzihhwgaqlrb58f9j56gfpz0va5ysm4";
  };

in {
  inherit nixpkgs shell shells solc solana solana-rust-bpf solana-llvm spl
    ethereum-client-prog
    ethereum-client-no-prog
    ethereum-client-dep-srcs
    generate-solana-genesis
    solana-client-tool
    run-solana-testnet
    ethashproof
  ;
  inherit (nixpkgs.haskellPackages) solana-bridges;
}
