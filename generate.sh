#! /bin/bash

# Refresh all of the automatically generated files

nix develop --command bash -c "aiken build ./src/onchain/validators --out ./generated/scripts/plutus.json"