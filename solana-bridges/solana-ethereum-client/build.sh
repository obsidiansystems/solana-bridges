export CC=$SOLANA_LLVM_CC
export AR=$SOLANA_LLVM_AR
rustup toolchain link bpf $(rustc --print sysroot)
xargo build --target bpfel-unknown-unknown --release --no-default-features --features program
