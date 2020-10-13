{ config, pkgs, ... }:

{
  services.fprintd = {
    enable = true;
    package = pkgs.fprintd-thinkpad;
  };
}
