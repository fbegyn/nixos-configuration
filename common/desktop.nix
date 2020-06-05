{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Hardware
    lm_sensors
    iftop
    htop
    # Utilities
    tmux
    vpnc
    pass
    xclip
    direnv
  ];

  virtualisation.docker.enable = true;
}
