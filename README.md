# Solana bridges

- [Setup](#setup)
- [Solana to Ethereum](#solana-to-ethereum-bridge)
- [Ethereum to Solana](#ethereum-to-solana-bridge)

## Setup
The Nix package manager is used for handling dependencies.
The main build can take over 10 minutes the first time it's run, but it should be much faster afterwards.

```shell
git clone https://github.com/obsidiansystems/solana-bridges.git
cd solana-bridges
nix-build
```

#### Cleanup
The following directories will be populated by various tools used by this repo:
  - `~/.ethash`
  - `~/.ethashproof`
  - `~/.config/solana`
  - `~/.cargo`
  - `~/.xargo`
  
Take caution when removing them since other programs might also be using these directories.

#### Run an ethereum testnet locally
On a separate terminal
```shell
make run-ethereum-testnet
```
Leave the testnet running on this terminal

#### Run a solana testnet locally
On a separate terminal

```shell
make run-solana-testnet
```
Leave the testnet running on this terminal

## Solana to Ethereum bridge

#### Deploy contract
```shell
$(nix-build -A solana-bridges)/bin/deploy-solana-client > solana-to-ethereum-config.json
```
Output should end with something similar to

`Contract deployed at address: 0xCb15617c1190448F318b8179263a72deF2EE782a`

#### Start relayer
```shell
$(nix-build -A solana-bridges)/bin/relay-solana-to-ethereum solana-to-ethereum-config.json
```

## Ethereum to Solana bridge

#### Create and fund an account
```shell
$(nix-build -A solana)/bin/solana-keygen new --no-passphrase
$(nix-build -A solana)/bin/solana airdrop 1000 --faucet-host 127.0.0.1
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
