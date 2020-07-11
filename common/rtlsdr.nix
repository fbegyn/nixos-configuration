{ config, pkgs, ... }:

{
  services.udev.packages = [
    pkgs.rtl-sdr
  ];
  boot.blacklistedKernelModules = [ "dvb_usb_rtl28xxu" ];
}
