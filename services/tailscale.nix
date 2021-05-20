{ config, pkgs, lib, ... }:

let
  cfg = config.thecy.services.tailscale;
in
with lib; {
  options.thecy.services.tailscale = {
    enable = mkEnableOption "enables tailscale client services";

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.tailscale;
      example = "pkgs.tailscale";
      description = "pkg to use for tailscale";
    };

    notifySupport = mkEnableOption "Enables systemd-notify support";

    autoprovision = {
      enable = mkEnableOption "enable auto provisioning";
      key = mkOption {
        type = types.str;
        description = "API secret used to access tailscale";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
    systemd.packages = [ cfg.package ];

    systemd.services.tailscale = {
      description = "Tailscale client daemon";

      after = [ "network-pre.target" ];
      wants = [ "network-pre.target" ];
      wantedBy = [ "multi-user.target" ];

      unitConfig = {
        StartLimitIntervalSec = 0;
        StartLimitBurst = 0;
      };

      serviceConfig = {
        ExecStart = "${cfg.package}/bin/tailscaled --port ${toString cfg.port}";

        RuntimeDirectory = "tailscale";
        RuntimeDirectoryMode = 755;

        StateDirectory = "tailscale";
        StateDirectoryMode = 750;

        CacheDirectory = "tailscale";
        CacheDirectoryMode = 750;

        Restart = "on-failure";
      } // (mkIf cfg.notifySupport {
        ExecStart = "${cfg.package}/bin/tailscaled --port ${toString cfg.port}";

        RuntimeDirectory = "tailscale";
        RuntimeDirectoryMode = 755;

        StateDirectory = "tailscale";
        StateDirectoryMode = 750;

        CacheDirectory = "tailscale";
        CacheDirectoryMode = 750;

        Restart = "on-failure";
        Type = "notify";
      });
    };

    systemd.services.tailscale-autoprovision = mkIf cfg.autoprovision.enable {
      description = "automagically provision a tailscale machine";
      after = [ "network-pre.target" "tailscale.service" ];
      wants = [ "network-pre.target" "tailscale.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RuntimeDirectory = "tailscale";
        RuntimeDirectoryMode = 755;

        StateDirectory = "tailscale";
        StateDirectoryMode = 750;

        CacheDirectory = "tailscale";
        CacheDirectoryMode = 750;
      };

      script = ''
        ${cfg.package}/bin/tailscale up --authkey=${cfg.autoprovision.key}
      '';
    };
  };
}
