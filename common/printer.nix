{ config, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      gutenprint
      gutenprintBin
      brlaser
      brgenml1lpr
      brgenml1cupswrapper
      cnijfilter2
      hplip
      splix
      samsung-unified-linux-driver
    ];
  };
}
