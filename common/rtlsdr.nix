{ config, pkgs, ... }:

{
  services.udev.packages = [
    pkgs.unstable.rtl-sdr
  ];
  boot.blacklistedKernelModules = [ "dvb_usb_rtl28xxu" ];

  users.users.francis.extraGroups = [ "plugdev" ];

  home-manager.users.francis.home.packages = with pkgs.unstable; [
    gqrx
    urh
    rtl-sdr
  ];
  hardware.rtl-sdr.enable = true;

}
