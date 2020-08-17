{ config, pkgs, ...}:

{
  services.corerad = {
    enable = true;
    configFile = "./config.toml";
  }
}
