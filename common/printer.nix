{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.avahi = {
    enable = true;
    nssmdns = true;
  };
  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprintBin
      pkgs.brlaser
      pkgs.brgenml1lpr
      pkgs.brgenml1cupswrapper
    ];
  };
}
