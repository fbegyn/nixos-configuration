{ config, pkgs, ...}:

{
  services.prometheus.exporters= {
    blackbox = {
      configFile = ./blackbox.yml;
    };
  };
}
