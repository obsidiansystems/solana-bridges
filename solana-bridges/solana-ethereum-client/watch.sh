nix-shell ../../default.nix -A shell-x86 --run "CARGO_TARGET_DIR='target-x86' cargo watch -c -x 'test --features program'"
