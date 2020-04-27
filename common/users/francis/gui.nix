{ config, pkgs, ... }:

{
  imports = [
    ./i3
  ];

  home.packages = with pkgs; [
    # Comms
    slack
    mattermost-desktop
    # Browser
    firefox
    qutebrowser
    # Utilities
    youtube-dl
    mpv
    rofi
    rofi-pass
    # Games
    steam
  ];
}
