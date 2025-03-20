#! /usr/bin/env nix
#! nix shell nixpkgs#fd nixpkgs#fourmolu --command bash

fd --extension hs --exclude 'dist-newstyle/*' --exclude 'dist/*' --exclude '.stack-work/*' --exec bash -c "fourmolu --quiet --mode inplace {}"
