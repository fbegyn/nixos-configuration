{ config, pkgs, ... }:

{
  hardware = {
    bluetooth.enable = true;
    pulseaudio= {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
  };
  environment.systemPackages = with pkgs; [
    bluez
  ];
}
