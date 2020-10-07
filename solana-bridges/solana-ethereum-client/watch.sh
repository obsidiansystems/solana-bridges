#!/usr/bin/env sh
nix-shell ../../default.nix -A shells.target-x86 --run "CARGO_TARGET_DIR='target-x86' cargo watch -c -x 'test --features program -- -Z unstable-options --report-time'"
