{ pkgs, ... }:

{
  home-manager.users.francis.home.packages = [
    pkgs.unstable.teamspeak_client
  ];
}

