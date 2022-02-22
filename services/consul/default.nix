{ config, pkgs, ...}:

{
  services.consul = {
    package = pkgs.unstable.consul;
    enable = true;
    webUi = true;
  };
}
