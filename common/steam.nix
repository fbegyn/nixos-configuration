{ config, pkgs, ... }:

{
  # make steam work
  programs.steam.enable = true;
  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };
}

