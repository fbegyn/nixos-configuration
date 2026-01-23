#! /usr/bin/env bash
set -xeo pipefail

FLAKE=$1
TARGET=$2

OPTIONS=$3

nixos-rebuild switch \
	--no-reexec \
	--flake ${FLAKE} \
	--target-host ${TARGET} \
	${OPTIONS} \
	-j 4 --cores 4
