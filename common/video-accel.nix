{ config, pkgs, ... }:

{
  # enable video acceleration
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      # intel
      intel-media-driver
      # vaapiIntel
      vpl-gpu-rt
      # amd
      libva-vdpau-driver
      libvdpau-va-gl
    ];
  };
}

