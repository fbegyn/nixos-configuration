{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.mpv
    master.youtube-dl
    master.streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
