#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solana-client-evm -i bash

set -uo pipefail
cd "$(dirname $0)"

solc --optimize --abi --bin SolanaClient.sol -o dist --overwrite
