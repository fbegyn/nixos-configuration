{ config, pkgs, ...}:

{
  services.corerad = {
    configFile = ./config.toml;
    package = pkgs.unstable.corerad;
  };
}
