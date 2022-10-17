{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  hardware.sane = {
    enable = true;
    brscan4 = {
      enable = true;
      netDevices = {
        home = { model = "DCP-L3550CDW"; ip = "10.3.5.20"; };
      };
    };
  };
}
