#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solidity -i bash

set -uo pipefail

solc --optimize --abi --bin SolanaClient.sol -o dist --overwrite
