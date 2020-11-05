{ config, pkgs, ...}:

{
  services.traefik = {
    enable = true;
    dynamicConfigFile = ./dynamic.toml;
    staticConfigFile = ./static.toml;
  };
}
