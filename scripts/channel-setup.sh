#! /usr/bin/env bash
set -eu

#switch to the branch of the nixos OS
if [ $# -gt 1 ]; then
	if [ $1 == "workstation" ]; then
		nix-channel --add https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz nixos
		nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
	elif [ $1 == "server" ]; then
		nix-channel --add https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz nixos
		nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
	else
		nix-channel --add https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz nixos
		nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
	fi
else
	nix-channel --add https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz nixos
	nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
fi

# setup nixos-hardware and home-manager
nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
nix-channel --add https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable.tar.gz unstable
nix-channel --add https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable-small.tar.gz unstable-small

# update the channels
nix-channel --update
