#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solana-client-evm -i bash

set -uo pipefail

root="$(git rev-parse --show-toplevel)"

watchdirs=("$(dirname $0)")

while true; do
    clear
    solc SolanaClient.sol
    if ! inotifywait -qre close_write "${watchdirs[@]}"; then
        exit "inotifywait failed"
    fi
    echo
done
