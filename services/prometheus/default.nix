{ config, pkgs, lib, ... }:

let
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
with lib; {
  options.francis.services.prometheus = {
    retention = {
      time = mkOption {
        type = lib.types.str;
        default = "30d";
        example = "365d";
        description = "Retention time passed to the Prometheus TSDB";
      };
      size = mkOption {
        type = lib.types.str;
        default = "0";
        example = "10GB";
        description = "Retention size passed to the Prometheus TSDB, defaults to 0 disabling it";
      };
    };
  };
  config.services.prometheus = let
    vars = import ../../secrets/hosts.nix;
    cfg = config.francis.services.prometheus;
  in {
    extraFlags = [
      "--storage.tsdb.retention.time ${cfg.retention.time}"
      "--storage.tsdb.retention.size ${cfg.retention.size}"
      "--web.enable-admin-api"
      "--web.enable-lifecycle"
    ];
    alertmanager = {
      configuration = {
        global = {
          smtp_from = "monitoring@begyn.be";
          smtp_smarthost = "mail.begyn.be:587";
          smtp_hello = "eos.begyn.be";
          smtp_auth_username = "bots@begyn.be";
          smtp_auth_password = "${vars.mail-01.mailserver.botsPass}";
        };
        receivers = [{
          name = "default";
          email_configs = [{
            to = "francis@begyn.be";
          }];
        }];
      };
    };
  };
}
