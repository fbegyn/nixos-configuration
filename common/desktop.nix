{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  nixpkgs.config.allowBroken = true;
  environment.systemPackages = with pkgs; [
    slack
    mattermost-desktop
    qutebrowser
    mpv
    youtube-dl
    htop
    rofi
    fzf
    rofi-pass
    pass
  ];

  francis = {
    gui.enable = true;
  };

  home-manager.users.francis = (import ./users/francis/gui.nix);
}
