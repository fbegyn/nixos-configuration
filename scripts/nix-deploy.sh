#! /usr/bin/env bash
NIX_SSHOPTS="-t"

FLAKE=$1
TARGET=$2

JOBS={$3:-2}
CORES={$4:-4}

nixos-rebuild switch \
	--fast \
	--flake ${FLAKE} \
	--target-host ${TARGET} \
	--use-remote-sudo \
	-j 4 --cores 4
