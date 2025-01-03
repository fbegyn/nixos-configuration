{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    mpv
    yt-dlp
    open-in-mpv
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
