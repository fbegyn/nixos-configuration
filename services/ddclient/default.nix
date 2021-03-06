{ config, ... }:

{
  services.ddclient = let
    vars = import ../../secrets/variables.nix;
  in {
    enable = true;
    interval = "5m";
    protocol = "cloudflare";
    username = "${vars.cf.email}";
    password = "${vars.cf.dyndns.key}";
  };
}
