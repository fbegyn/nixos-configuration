{ config, lib, pkgs, ... }:

{
  services.usbmuxd.enable = true;
  services.usbmuxd.user = "francis";
  environment.systemPackages = with pkgs; [
    libimobiledevice
    usbmuxd
  ];
}
