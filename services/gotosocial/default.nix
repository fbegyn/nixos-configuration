{ config, lib, pkgs, ... }:
with lib;
let
  serviceName = "gotosocial";
  pkg = import ../../pkgs/gotosocial.nix { inherit pkgs; };
  cfg = config.fbegyn.services.gotosocial;
in {
  options.fbegyn.services.gotosocial = {
    enable = mkEnableOption "enables hosting of gotosocial";
    serverName = mkOption {
      type = types.str;
      default = "social.example.com";
      example = "social.example.com";
      description = "gotosocial server name";
    };
    user = mkOption {
      type = types.str;
      default = "gotosocial";
      example = "gotosocial";
      description = "user to run gotosocial as";
    };
    address = mkOption {
      type = types.str;
      default = "localhost";
      example = "0.0.0.0";
      description = "address to bind gotosocial to";
    };
    port = mkOption {
      type = types.port;
      default = 9755;
      example = 9755;
      description = "port number for gotosocial";
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/gotosocial";
      example = "localhost";
      description = "address to bind gotosocial to";
    };
    ACMEHost = mkOption {
      type = types.str;
      default = "example.com";
      example = "example.com";
      description = "ACME host to use for the NGINX proxy";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.user;
      createHome = true;
      home = cfg.dataDir;
    };
    users.groups.${cfg.user} = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir}/data 1770 ${cfg.user} ${cfg.user}"
      "d ${cfg.dataDir}/media 1770 ${cfg.user} ${cfg.user}"
    ];

    systemd.services.${serviceName} = let
      yaml = pkgs.formats.yaml { };
      configFile = yaml.generate "${serviceName}.yaml" {
        host = cfg.serverName;
        bind-address = cfg.address;
        port = cfg.port;
        db-type = "sqlite";
        db-address = "${cfg.dataDir}/data/sqlite.db";
        web-template-base-dir = "${pkg}/web/template/";
        web-asset-base-dir = "${pkg}/web/assets/";
        accounts-registration-open = false;
        storage-local-base-path = "${cfg.dataDir}/media";
      };
    in {
      enable = true;
      description = "${serviceName} service";
      restartIfChanged = true;
      restartTriggers = [ configFile pkg ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = cfg.user;
        Group = cfg.user;
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${pkg}/bin/gotosocial --config-path ${configFile} server start";
      };
    };
  };
}
