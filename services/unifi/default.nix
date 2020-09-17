{ pkgs, ... }:

{
  services.unifi = {
    enable = true;
    unifiPackage = pkgs.unstable.unifi;
  };
}
