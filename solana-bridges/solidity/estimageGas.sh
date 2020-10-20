#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solidity -i bash

set -uo pipefail

OUTPUT=$(solc --optimize --gas SolanaClient.sol)
echo "$OUTPUT"
echo ""
if (echo "$OUTPUT" | grep -q "infinite\$"); then
    # https://ethereum.stackexchange.com/a/39221
    echo "Note: any backward jumps or loops in the assembly code will report infinite gas";
fi;
