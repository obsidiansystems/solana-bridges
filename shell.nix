let
  inherit (import ./.) nixpkgs solana;
in nixpkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = [ solana ];
}
