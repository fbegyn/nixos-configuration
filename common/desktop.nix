{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Hardware
    lm_sensors
    iftop
    htop
    # Utilities
    tmux
    pass
    direnv
  ];

  virtualisation.docker.enable = true;
}
