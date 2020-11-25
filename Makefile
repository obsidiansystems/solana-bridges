all:
	nix-build

watch:
	nix-shell --run "cd solana-bridges && ghcid -c 'cabal new-repl' --restart solana-bridges.cabal --restart 'solana-client/SolanaClient.sol'"

test:
	nix-shell --run "cd solana-bridges && ghcid -c 'cabal new-repl' --restart solana-bridges.cabal --restart 'solana-client/SolanaClient.sol' --test 'Solana.Utils.testSolanaClient'"

repl:
	nix-shell --run "cd solana-bridges && cabal new-repl"
