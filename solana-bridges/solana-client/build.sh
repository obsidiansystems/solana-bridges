#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solana-client-evm -i bash

set -uo pipefail

solc --optimize --abi --bin SolanaClient.sol -o dist --overwrite
