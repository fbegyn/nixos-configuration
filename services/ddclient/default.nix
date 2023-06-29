{ config, ... }:

{
  services.ddclient = let
    vars = import ../../secrets/variables.nix;
  in {
    interval = "10m";
    passwordFile = "/etc/ddclient.key";
  };
}
