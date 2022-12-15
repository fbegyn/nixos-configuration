{ config, pkgs, lib, ... }:

let
  cfg = config.fbegyn.services.tailscale;
in
with lib; {
  options.fbegyn.services.tailscale = {
    enable = mkEnableOption "enables tailscale client services";

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.tailscale;
      example = "pkgs.tailscale";
      description = "pkg to use for tailscale";
    };

    port = mkOption {
      type = types.port;
      default = 41641;
      example = "8113";
      description = "The port to listen on for tunnel traffic (0=autoselect).";
    };

    notifySupport = mkOption {
      type = types.bool;
      default = false;
      example = "true";
      description = "Enable notifyd support";
    };

    autoprovision = {
      enable = mkEnableOption "enable auto provisioning";
      key = mkOption {
        type = types.str;
        description = "API secret used to access tailscale";
      };
      cmd = mkOption {
        type = types.str;
        default = "${cfg.package}/bin/tailscale up --authkey=${cfg.autoprovision.key} ${lib.concatStringsSep " " cfg.options}";
        description = "Command to use when running Tailscale";
      };
      options = mkOption {
        type = types.listOf types.str;
        default = [];
        example = "[ \"--advertise-exit-node\" ]";
        description = "Options to pass to Tailscale";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
    systemd.packages = [ cfg.package ];

    systemd.services.tailscale = {
      enable = true;
      description = "Tailscale client daemon";
      path = [ pkgs.openresolv ];
      after = [ "network-pre.target" ];
      wants = [ "network-pre.target" ];
      wantedBy = [ "multi-user.target" ];

      unitConfig = {
        StartLimitIntervalSec = 0;
        StartLimitBurst = 0;
      };

      serviceConfig = {
        ExecStart = cfg.cmd;
        RuntimeDirectory = "tailscale";
        RuntimeDirectoryMode = 755;
        StateDirectory = "tailscale";
        StateDirectoryMode = 750;
        CacheDirectory = "tailscale";
        CacheDirectoryMode = 750;
        Restart = "on-failure";
      };
    };

    systemd.services.tailscale-autoprovision = mkIf cfg.autoprovision.enable {
      description = "automagically provision a tailscale machine";
      after = [ "network-pre.target" "tailscale.service" ];
      wants = [ "network-pre.target" "tailscale.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      script = ''
        # authenticate to tailscale
        ${cfg.cmd}
      '';
    };
  };
}
