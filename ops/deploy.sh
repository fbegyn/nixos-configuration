#! /usr/bin/env nix-shell
#! nix-shell -p morph -i bash

set -e

morph build --keep-result $1
morph push $1

# morph deploy $1 switch
