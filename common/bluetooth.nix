{ config, pkgs, ... }:

{
  hardware = {
    bluetooth.enable = true;
    pulseaudio= {
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
  };
  environment.systemPackages = with pkgs; [
    bluez
  ];
}
