{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      mpv
      youtube-dl
    ];

    xdg.configFile."mpv/mpv.conf".source = ./mpv.conf;
  };
}
