{ pkgs, ... }:

{
  home.packages = with pkgs; [
    master.mpv
    master.youtube-dl
    unstable.streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
