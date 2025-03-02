{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
    "https://github.com/j-mueller/sc-tools"."3b760d6ddb1ff7c835687f599260e0a45c54dbe3" = "sha256-erdlKWDN6/q0MSn7KnksiRP7J9K1KUIMIsSpfugu9PY=";
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "batcherless-offchain";
    compiler-nix-name = "ghc966";
    # index-state = "2024-10-16T00:00:00Z";
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project
