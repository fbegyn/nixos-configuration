{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    mpv
    youtube-dl
    castnow
    streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
