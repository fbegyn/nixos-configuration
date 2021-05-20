{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.gutenprintBin
    pkgs.brlaser
    pkgs.brgenml1lpr
    pkgs.brgenml1cupswrapper
  ];
}
