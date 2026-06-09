{ config, pkgs, ... }:

{
  # make steam work
  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      extraPkgs = pkgs: with pkgs; [
        libkrb5
        keyutils
        stdenv.cc.cc.lib
        libvorbis
        libpng
        libxcursor
        libxi
        libxinerama
        libxscrnsaver
      ];
    };
    gamescopeSession.enable = true;
    extraPackages = with pkgs; [
      gamescope
      libkrb5
      keyutils
    ];
  };
  programs.gamescope = {
    enable = true;
    capSysNice = false;
  };
}

