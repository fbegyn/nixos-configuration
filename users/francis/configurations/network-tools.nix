{ config, pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.traceroute
      unstable.ldns
      unstable.mtr
    ];
  };
}
