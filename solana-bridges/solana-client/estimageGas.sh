#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solana-client-evm -i bash

set -uo pipefail
cd "$(dirname $0)"

OUTPUT=$(solc --optimize --gas SolanaClient.sol)
echo "$OUTPUT"
echo ""
if (echo "$OUTPUT" | grep -q "infinite\$"); then
    # https://ethereum.stackexchange.com/a/39221
    echo "Note: any backward jumps or loops in the assembly code will report infinite gas";
fi;
