all:
	nix-build

watch:
	nix-shell --run "cd solana-bridges && ghcid -c 'cabal new-repl' --restart solana-bridges.cabal"

repl:
	nix-shell --run "cd solana-bridges && cabal new-repl"
