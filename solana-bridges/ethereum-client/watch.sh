#! /usr/bin/env nix-shell
#! nix-shell ../../default.nix -A shells.ethereum-client-x86 -i bash

set -uo pipefail
cd "$(dirname $0)"

RUST_BACKTRACE=1 \
	RUST_LOG=quickcheck \
	CARGO_TARGET_DIR='target-x86' \
	cargo watch -c -x 'test --features program -- -Z unstable-options --report-time'
