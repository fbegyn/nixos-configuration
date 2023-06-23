{ config, ... }:

{
  services.ddclient = let
    vars = import ../../secrets/variables.nix;
  in {
    interval = "15m";
    protocol = "cloudflare";
    username = "${vars.cf.email}";
    passwordFile = "/etc/ddclient.key";
  };
}
