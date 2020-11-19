#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.ethereum-client-bpf -i bash
set -uo pipefail

if [ "$#" -ne 1 ] ; then
    echo "Usage: $0 <ETHEREUM-TO-SOLANA-CONFIG>.json"
    exit 1
fi

CONFIG_FILE=$1
shift
ACCOUNT=$(jq -r .accountId < $CONFIG_FILE)

watch -n 1 "solana account $ACCOUNT --output json | jq -r '.account.data[0]' | base64 -d | hexdump -C"
