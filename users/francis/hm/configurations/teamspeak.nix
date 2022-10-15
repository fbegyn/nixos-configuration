{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    teamspeak_client
  ];
}

