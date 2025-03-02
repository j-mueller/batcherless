{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "batcherless";
  packages = [
    pkgs.ghcid
    pkgs.nixpkgs-fmt
    pkgs.aiken
  ];

  env = { };


  preCommit = {
    # NOTE: when this attribute set changes, `.pre-commit-config.yaml` (which is a sym link to the nix store) changes.
    #       To maintain a the same hooks for both nix and non-nix environment you should update the `.pre-commit-config.yaml.nonix`
    #       (`cp .pre-commit-config.yaml .pre-commit-config.yaml.nonix`).
    #       This step is necessary because `.pre-commit-config.yaml` is ignored by git.
    cabal-fmt.enable = true;
    stylish-haskell.enable = true;
    nixpkgs-fmt.enable = false;
  };
}
