# Shared baseline for the headless machines (eos, selene, nix-builder-01).
{ ... }:

{
  imports = [
    ../common/gpg.nix
    ../common/network-tools.nix

    ../users
    ../users/francis
  ];
}
