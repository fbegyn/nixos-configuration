{ pkgs, ... }:

let
  unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable-small/nixexprs.tar.xz) {};
in
{
  nixpkgs.overlays = [
    (_self: _super: {
      loki = unstable.loki;
    })
  ];

  services.loki = {
    enable = true;
    configFile = ./loki.yml;
  };

  users.groups.promtail.gid = 2000;
  users.users.promtail = {
    isSystemUser = true;
    extraGroups = [ "systemd-journal" ];
  };

  systemd.services.promtail = {
    description = "Promtail";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.grafana-loki}/bin/promtail --config.file ${./promtail.yml}
      '';
      User = "promtail";
      Group = "promtail";
    };
  };
}
