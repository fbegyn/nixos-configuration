{ pkgs, ... }:

{

  home.packages = with pkgs; [
    maim
  ];

  home = {
    file.".config/i3/config".source = ./config;
  };
}
