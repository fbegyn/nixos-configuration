{ pkgs, ... }:

{

  home.packages = with pkgs; [
    mpv
    youtube-dl
  ];

  home.file.".config/mpv/mpv.conf".source = ./mpv.conf;
}
