{ pkgs, ... }:

{

  home.packages = with pkgs; [
    maim
    picom
    feh
  ];

  home = {
    file.".config/i3/config".source = ./config;
  };
}
