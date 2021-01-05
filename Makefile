all:
	nix-build

repl:
	nix-shell --run "cd solana-bridges && cabal new-repl"

watch:
	nix-shell --run "cd solana-bridges && ghcid -c 'cabal new-repl' --restart solana-bridges.cabal --restart 'solana-client/SolanaClient.sol'"

run-ethereum-testnet:
	$$(nix-build -A solana-bridges)/bin/run-ethereum-testnet

run-solana-testnet:
	$$(nix-build -A solana)/bin/solana config set --url http://localhost:8899
	$$(nix-build -A run-solana-testnet)/bin/run-solana-testnet solana-bridges/solana/genesis.tar.bz2

test-solana-client:
	nix-shell --run "cd solana-bridges && ghcid -c 'cabal new-repl' --restart solana-bridges.cabal --restart 'solana-client/SolanaClient.sol' --test 'Solana.Utils.testSolanaClient'"
