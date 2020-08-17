{ config, pkgs, ...}:

{
  services.corerad = {
    enable = true;
    configFile = ./config.toml;
    package = pkgs.unstable.corerad;
  };
}
