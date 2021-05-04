{ pkgs, ... }:

let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable-small.tar.gz) {};
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
