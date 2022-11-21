{ pkgs, ... }:

{
  services.grafana = {
    enable = true;
    addr = "";
    port = 3000;
    protocol = "http";
    dataDir = "/var/lib/grafana";
    package = pkgs.grafana;
  };
}
