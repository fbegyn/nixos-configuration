{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    mpv
    python311Packages.yt-dlp
    streamlink
  ];

  xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
}
