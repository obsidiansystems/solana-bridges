#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.solidity -i bash

set -uo pipefail

root="$(git rev-parse --show-toplevel)"

watchdirs=("$root/solana-bridges/solidity")

while true; do
    clear
    solc HelloWorld.sol
    if ! inotifywait -qre close_write "${watchdirs[@]}"; then
        exit "inotifywait failed"
    fi
    echo
done
