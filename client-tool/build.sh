#!/usr/bin/env nix-shell
#! nix-shell -I nixpkgs=../dep/nixpkgs/default.nix -i bash -p nodePackages.node2nix

node2nix --bypass-cache --pkg-name 'nodejs-12_x' --lock ./package-lock.json 

patch -p0 <<EOF
--- default.nix
+++ default.nix
@@ -14,4 +14,5 @@
 import ./node-packages.nix {
   inherit (pkgs) fetchurl fetchgit;
   inherit nodeEnv;
-}
\ No newline at end of file
+  globalBuildInputs = [pkgs.nodePackages.node-gyp-build];
+}
EOF
