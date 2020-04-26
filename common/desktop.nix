{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  environment.systemPackages = with pkgs; [
    slack
    mattermost-desktop
    firefox
    mpv
    htop
    fzf
    pass
  ];

  francis = {
    gui.enable = true;
  };

  home-manager.users.francis = (import ./users/francis/gui.nix);
}
