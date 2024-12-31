{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    mpv
    yt-dlp
    streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
