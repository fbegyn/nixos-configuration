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

    sockPath = mkOption {
      type = types.path;
      default = "/run/tailscale/tailscaled.socket";
      example = "/run/tailscale/tailscaled.socket";
      description = "The port to listen on for tunnel traffic (0=autoselect).";
    };

    statePath = mkOption {
      type = types.path;
      default = "/var/lib/tailscale/tailscaled.state";
      example = "/var/lib/tailscale/tailscaled.state";
      description = "The port to listen on for tunnel traffic (0=autoselect).";
    };

    notifySupport = mkOption {
      type = types.bool;
      default = false;
      example = "true";
      description = "Enable notifyd support";
    };

    cmd = mkOption {
      type = types.str;
      default = "${cfg.package}/bin/tailscaled --state=${cfg.statePath} --socket=${cfg.sockPath} --port ${toString cfg.port}";
      description = "Command to run tailscaled with";
    };

    autoprovision = {
      enable = mkEnableOption "enable auto provisioning";
      key = mkOption {
        type = types.str;
        description = "API secret used to access tailscale";
      };
      cmd = mkOption {
        type = types.str;
        default = "${cfg.package}/bin/tailscale up --auth-key=${cfg.autoprovision.key} ${lib.concatStringsSep " "cfg.autoprovision.options}";
        description = "Command to use when running Tailscale";
      };
      options = mkOption {
        type = types.listOf types.str;
        default = [];
        example = "[ \"--advertise-exit-node\" ]";
        description = "Options to pass to Tailscale";
      };
    };

    setSysctlForwarding = mkOption {
      type = types.bool;
      default = false;
      description = "Set the sysctl parameters for ip forwarding";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
    systemd.packages = [ cfg.package ];

    systemd.services.tailscaled = {
      enable = true;
      description = "Tailscale node agent";
      documentation = "https://tailscale.com/kb/";
      path = [ pkgs.openresolv ];
      after = [
        "network-pre.target"
        "NetworkManager.service"
        "systemd-resolved.service"
      ];
      wants = [ "network-pre.target" ];
      wantedBy = [ "multi-user.target" ];

      unitConfig = {
        StartLimitIntervalSec = 0;
        StartLimitBurst = 0;
      };

      serviceConfig = {
        ExecStart = cfg.cmd;
        ExecStartPre = "${cfg.package}/bin/tailscaled --cleanup";
        ExecStopPost = "${cfg.package}/bin/tailscaled --cleanup";
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
        ${cfg.autoprovision.cmd}
      '';
    };

    boot.kernel.sysctl = mkIf cfg.setSysctlForwarding {
      "net.ipv4.ip_forward" = "1";
      "net.ipv6.conf.all.forwarding" = "1";
    };
  };
}
