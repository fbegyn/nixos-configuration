{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    mpv
    yt-dlp
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
