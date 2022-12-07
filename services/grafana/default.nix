{ pkgs, ... }:

{
  services.grafana = {
    enable = true;
    settings.server = {
      http_port = 3000;
      http_addr = "";
      protocol = "http";
    };
    dataDir = "/var/lib/grafana";
    package = pkgs.grafana;
  };
}
