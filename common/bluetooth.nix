{ config, pkgs, ... }:

{
  hardware = {
    bluetooth.enable = true;
    pulseaudio= {
      enable = false;
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
  };
  environment.systemPackages = with pkgs; [
    bluez
  ];
}
