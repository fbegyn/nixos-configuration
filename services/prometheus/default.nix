{ ... }:

let
  unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable-small/nixexprs.tar.xz) {};

  staticScrape = (job_name: scheme: targets: {
    inherit job_name;
    inherit scheme;
    static_configs = [{ inherit targets; }];
  });
  consulScrape = (job_name: scheme: services: {
    inherit job_name;
    inherit scheme;
    consul_sd_configs = [
      {
        inherit services;
        server = "10.3.10.10:8500";
      }
    ];
  });
in
{
  nixpkgs.overlays = [
    (_self: _super: {
      prometheus = unstable.prometheus;
    })
  ];

  services.prometheus = {
    enable = true;
    extraFlags = [
      "--storage.tsdb.retention.time 720h"
      "--web.enable-admin-api"
      "--web.enable-lifecycle"
    ];
    alertmanager = {
      enable = true;
      configuration = {
        route = {
          group_by = ["alertname"];
          group_wait = "10s";
          group_interval = "10s";
          repeat_interval = "1h";
          receiver = "default";
        };
        receivers = [{
          name = "default";
        }];
      };
    };
    alertmanagers = [{
      static_configs = [{
        targets = ["localhost:9093"];
      }];
    }];
    globalConfig = {
      scrape_interval = "30s";
      scrape_timeout = "15s";
    };
  };
}
