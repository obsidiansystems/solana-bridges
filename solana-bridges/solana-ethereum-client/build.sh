export CC=$SOLANA_LLVM_CC
export AR=$SOLANA_LLVM_AR
xargo build --target bpfel-unknown-unknown --release --no-default-features --features program
