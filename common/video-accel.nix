{ config, pkgs, ... }:

{
  # enable video acceleration
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
    driSupport32Bit = true;
  };
}

