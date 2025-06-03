{ config, pkgs, ... }:

{
  hardware = {
    bluetooth.enable = true;
  };
  services.pulseaudio = {
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };
  environment.systemPackages = with pkgs; [
    bluez
  ];
}
