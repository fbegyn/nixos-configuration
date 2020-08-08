{ ... }:

let
  unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable-small/nixexprs.tar.xz) {};
in
{
  nixpkgs.overlays = [
    (_self: _super: {
      grafana = unstable.grafana;
    })
  ];
  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    port = 3000;
    domain = "begyn.lan";
    protocol = "http";
    dataDir = "/var/lib/grafana";
  };
}
