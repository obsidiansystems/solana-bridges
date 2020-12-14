#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solana-client-evm -i bash

set -uo pipefail
cd "$(dirname $0)"

root="$(git rev-parse --show-toplevel)"

watchdirs=("$(pwd)")

while true; do
    clear
    solc --optimize SolanaClient.sol
    if ! inotifywait -qre close_write "${watchdirs[@]}"; then
        exit "inotifywait failed"
    fi
    echo
done
