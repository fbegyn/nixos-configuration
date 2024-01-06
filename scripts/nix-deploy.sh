#! /usr/bin/env bash
set -xeuo pipefail

NIX_SSHOPTS="-t"

FLAKE=$1
TARGET=$2

nixos-rebuild switch \
	--fast \
	--flake ${FLAKE} \
	--target-host ${TARGET} \
	--use-remote-sudo \
	-j 4 --cores 4
