{ pkgs, ... }:

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
{
  services.prometheus = let
    vars = import ../../secrets/hosts.nix;
  in {
    enable = true;
    extraFlags = [
      "--storage.tsdb.retention.time 365d"
      "--web.enable-admin-api"
      "--web.enable-lifecycle"
    ];
    alertmanager = {
      enable = true;
      configuration = {
        global = {
          smtp_from = "monitoring@begyn.be";
          smtp_smarthost = "mail.begyn.be:587";
          smtp_hello = "eos.begyn.be";
          smtp_auth_username = "bots@begyn.be";
          smtp_auth_password = "${vars.mail-01.mailserver.botsPass}";
        };
        route = {
          group_by = ["alertname"];
          group_wait = "10s";
          group_interval = "10s";
          repeat_interval = "1h";
          receiver = "default";
        };
        receivers = [{
          name = "default";
          email_configs = [{
            to = "francis@begyn.be";
          }];
        }];
      };
    };
    alertmanagers = [{
      static_configs = [{
        targets = ["localhost:9093"];
      }];
    }];
    globalConfig = {
      scrape_interval = "10s";
      scrape_timeout = "8s";
    };
  };
}
