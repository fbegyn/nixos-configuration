{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.mpv
    unstable.youtube-dl
    unstable.castnow
    unstable.streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
