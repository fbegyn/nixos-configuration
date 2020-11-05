{ config, pkgs, ...}:

{
  services.traefik = {
    package = pkgs.unstable.traefik;
    enable = true;
    dynamicConfigFile = ./dynamic.toml;
    staticConfigFile = ./static.toml;
  };
  system.activationScripts = {
    createTraefikLogDir = ''
      mkdir -p /var/log/traefik
      chown -R traefik:traefik /var/log/traefik
      chmod -R 774 /var/log/traefik
    '';
  };
}
