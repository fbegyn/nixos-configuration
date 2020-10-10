{ config, pkgs, ... }:

{
  services.fprintd = {
    enable = true;
    package = pkgs.unstable.fprintd-thinkpad;
  };
}
