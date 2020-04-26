{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  environment.systemPackages = with pkgs; [
    slack
    mattermost-desktop
    qutebrowser
  ];

  francis = {
    gui.enable = true;
  };

  home-manager.users.francis = (import ./users/francis/gui.nix);
}
