{ pkgs, ... }:

{
  services.unifi = {
    enable = true;
    unifiPackage = pkgs.unifi;
  };
}
