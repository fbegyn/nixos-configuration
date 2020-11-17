{ config, pkgs, ...}:

{
  virtualisation.oci-containers.containers = {
    "speedtest" = {
      image = "jraviles/prometheus_speedtest:latest";
      ports = [
        "9516:9516"
      ];
    };
  };
}
