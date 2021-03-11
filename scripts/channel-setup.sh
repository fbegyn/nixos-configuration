#! /usr/bin/env bash

nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware 
nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager 
nix-channel --update
