# Solana bridges

- [Setup](#setup)
- [Solana to Ethereum](#solana-to-ethereum-bridge)
- [Ethereum to Solana](#ethereum-to-solana-bridge)

## Setup

This can take over 10 minutes the first time it's run, but it should be much faster afterwards

```shell
git clone https://github.com/obsidiansystems/solana-bridges.git
cd solana-bridges
nix-build
```

#### Run ethereum testnet
On a separate terminal
```shell
$(nix-build -A solana-bridges)/bin/run-ethereum-testnet
```
Leave the testnet running on this terminal

#### Run solana testnet
On a separate terminal

```shell
$(nix-build -A solana)/bin/solana config set --url http://localhost:8899
$(nix-build -A run-solana-testnet)/bin/run-solana-testnet solana-bridges/solana/genesis.tar.bz2
```
Leave the testnet running on this terminal

### Solana to Ethereum bridge

#### Deploy contract
```shell
$(nix-build -A solana-bridges)/bin/deploy-solana-client > solana-to-ethereum-config.json
```
Output should end with something like `Contract deployed at address: 0xCb15617c1190448F318b8179263a72deF2EE782a`

#### Start relayer
```shell
$(nix-build -A solana-bridges)/bin/relay-solana-to-ethereum solana-to-ethereum-config.json
```

### Ethereum to Solana bridge

#### Create and fund an account
```shell
$(nix-build -A solana)/bin/solana-keygen new --no-passphrase
$(nix-build -A solana)/bin/solana airdrop 1000 --ws http://localhost:9900
```

#### Build contract
```shell
./solana-bridges/ethereum-client/build.sh
```

#### Deploy contract
```shell
./solana-bridges/ethereum-client/deploy.sh > ethereum-to-solana-config.json
```

#### Start relayer
```shell
$(nix-build -A solana-bridges)/bin/relay-ethereum-to-solana ethereum-to-solana-config.json
```
