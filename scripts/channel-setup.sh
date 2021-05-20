#! /usr/bin/env bash

#jswitch to the master branch of the nixpkgs
nix-channel --remove nixos
nix-channel --add https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz nixos

# setup nixos-hardware and home-manager
nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
nix-channel --add https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable.tar.gz nixpkgs
nix-channel --add https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable-small.tar.gz nixos-unstable-small

# update the channels
nix-channel --update
