{ ... }:

{
  services.grafana = {
    enable = true;
    addr = "";
    port = 3000;
    domain = "begyn.lan";
    protocol = "http";
    dataDir = "/var/lib/grafana";
  };
}
