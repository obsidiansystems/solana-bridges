{pkgs ? (import ../dep/nixpkgs {})} : (import ./. {inherit pkgs;}).shell
