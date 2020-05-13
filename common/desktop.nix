{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  environment.systemPackages = with pkgs; [
    # Hardware
    lm_sensors
    stress
    neofetch
    iftop
    htop
    # Utilities
    tmux
    alacritty
    vpnc
    pass
    xclip
    # Games
    # steam
    direnv
  ];

  virtualisation.docker.enable = true;

  francis = {
    gui.enable = true;
  };

  home-manager.users.francis = (import ./users/francis/gui.nix);
}
