{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.mpv
    unstable.youtube-dl
    streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
