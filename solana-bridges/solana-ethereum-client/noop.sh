#!/usr/bin/env sh

echo ""
echo "Deploying contract......"
echo ""
PROGRAM_ID=$(solana deploy target-bpf/bpfel-unknown-unknown/release/solana_ethereum_client.so --use-deprecated-loader | jq .programId -r)

echo ""
echo "Allocating space......"
echo ""

solana-bridge-tool alloc --program-id $PROGRAM_ID --space 99999 > ../relayer.json
STORAGE_ID=$(jq .accountId -r ../relayer.json)

echo ""
echo "Invoking noop instruction......"
echo ""
solana-bridge-tool call  --program-id $PROGRAM_ID --storage-id $STORAGE_ID --instruction 00
