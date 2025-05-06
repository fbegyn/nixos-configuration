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
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
}

