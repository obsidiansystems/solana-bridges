#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.scripts -i bash

set -uo pipefail
cd "$(dirname $0)"

PROGRAM_ID=$(solana deploy target-bpf/bpfel-unknown-unknown/release/solana_ethereum_client.so --use-deprecated-loader | jq .programId -r)
$(nix-build ../../default.nix -A solana-client-tool)/bin/solana-bridge-tool alloc --program-id $PROGRAM_ID --space 99999
