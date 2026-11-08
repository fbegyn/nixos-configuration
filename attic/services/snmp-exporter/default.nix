{ config, pkgs, ...}:

{
  services.prometheus.exporters= {
    snmp = {
      enable = true;
      openFirewall = true;
      configurationPath = /etc/snmp-exporter/snmp.yml;
    };
  };

  environment.etc = {
    "snmp-exporter/snmp.yml" = {
      enable = true;
      source = ./snmp.yml;
      group = "snmp-exporter";
      user = "snmp-exporter";
    };
  };
}

