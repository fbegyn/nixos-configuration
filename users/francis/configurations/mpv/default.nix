{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.mpv
      unstable.youtube-dl
      unstable.streamlink
    ];

    xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
  };
}
