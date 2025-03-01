#! /usr/bin/env nix
#! nix shell nixpkgs#fd nixpkgs#haskellPackages.cabal-fmt --command bash

fd --extension cabal --exclude 'dist-newstyle/*' --exclude 'dist/*' --exclude '.stack-work/*' --exec bash -c "cabal-fmt -i {} || true"