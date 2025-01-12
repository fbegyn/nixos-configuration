{ config, pkgs, ... }:

{
  # make steam work
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    extraPackages = with pkgs; [
      gamescope
      libkrb5
      keyutils
    ];
  };
  programs.gamescope = {
    enable = true;
    capSysNice = true;
  };
}

