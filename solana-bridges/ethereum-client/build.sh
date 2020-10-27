#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.ethereum-client-bpf -i bash

set -uo pipefail
cd "$(dirname $0)"

export CC=$SOLANA_LLVM_CC
export AR=$SOLANA_LLVM_AR
xargo build --target bpfel-unknown-unknown --release --no-default-features --features program
