{ config, pkgs, ... }:

{
  services.resolved = {
    enable = true;
    dnssec = "false";
  };
}
