## Setup

### Setup solana account
`$(nix-build -A solana)/bin/solana-keygen new --no-passphrase`
`$(nix-build -A solana)/bin/solana config set --url http://localhost:8899`

### Run solana testnet
`$(nix-build -A solana-testnet)/bin/run-solana-testnet`

#### Get funds from faucet
`$(nix-build -A solana)/bin/solana airdrop 1000 --ws http://localhost:9900`

### Run ethereum testnet
`$(nix-build -A solana-bridges)/bin/run-ethereum-testnet`

## Solana to Ethereum bridge

### Deploy contract
`$(nix-build -A solana-bridges)/bin/deploy-solana-client > solana-to-ethereum-relayer-config.json`

### Start relayer
`$(nix-build -A solana-bridges)/bin/relay-solana-to-ethereum solana-to-ethereum-relayer-config.json`

## Ethereum to Solana bridge

### Build contract
`./solana-bridges/ethereum-client/build.sh`

### Deploy contract
`PROGRAM_ID=$(`$(nix-build -A solana)/bin/solana deploy solana-bridges/ethereum-client/target-bpf/bpfel-unknown-unknown/release/solana_ethereum_client.so --use-deprecated-loader | jq .programId -r)`

### Allocate storage for contract
`$(nix-build -A solana-client-tool)/bin/solana-bridge-tool alloc --program-id $PROGRAM_ID --space 99999 > ethereum-to-solana-relayer-config.json`

### Start relayer
`$(nix-build -A solana-bridges)/bin/relay-ethereum-to-solana ethereum-to-solana-relayer-config.json`
