{ config, pkgs, ...}:

{
  services.prometheus.exporters= {
    blackbox = {
      enable = true;
      openFirewall = true;
      configFile = ./blackbox.yml;
    };
  };
}
