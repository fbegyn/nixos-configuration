{ config, pkgs, ... }:

{
  imports = [
    ./cachix.nix
    ./unstable.nix
    ./nur.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];
}
