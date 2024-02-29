linux:
	nixos-rebuild switch --flake ".#" --fast -j 4 --cores 3

mac:
	darwin-rebuild switch --flake ".#" -j 4 --cores 3
