{ config, pkgs, ... }:

{
  # enable video acceleration
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      # vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
    enable32Bit = true;
  };
}

