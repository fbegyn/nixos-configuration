{ config, pkgs, ... }: {
  imports = [
    ../../common/network-tools.nix
    ../../common/wireguard.nix
    ./fbegyn-srv.nix
  ];
}
