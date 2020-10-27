{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.traceroute
    unstable.ldns
    unstable.mtr
  ];
}
