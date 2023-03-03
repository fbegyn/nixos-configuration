{ pkgs, ... }:

let
  vars = import ../../secrets/variables.nix;
in {
  services.unifi = {
    enable = true;
    unifiPackage = pkgs.unstable.unifi;
    jrePackage = pkgs.jdk11;
    mongodbPackage = pkgs.mongodb-6_0;
  };

  services.prometheus.exporters.unifi = {
    enable = true;
    unifiAddress = "${vars.unifi.host}";
    unifiUsername = "${vars.unifi.username}";
    unifiPassword = "${vars.unifi.password}";
    unifiInsecure = true;
  };
}
