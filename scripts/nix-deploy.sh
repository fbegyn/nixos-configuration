#! /usr/bin/env bash
set -xeo pipefail

# NIX_SSHOPTS="-t"

FLAKE=$1
TARGET=$2

OPTIONS=$3

nixos-rebuild switch \
	--fast \
	--flake ${FLAKE} \
	--target-host ${TARGET} \
	${OPTIONS} \
	-j 4 --cores 4
