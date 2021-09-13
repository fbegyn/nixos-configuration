{ config, pkgs, lib, ...}:


let
  cfg = config.services.fluent-bit; 
in
with lib; {
  options.services.fluent-bit = {
    enable = mkEnableOption "Whether to enable fluent-bit";

    config = mkOption {
      type = types.lines;
      default = "";
      description = "Fluent-bit config.";
    };

    parsers = mkOption {
      type = types.lines;
      default = "";
      description = "Fluent-bit parsers config";
    };

    package = mkOption {
      type = types.path;
      default = pkgs.fluent-bit;
      defaultText = "pkgs.fluent-bit";
      description = "The fluent-bit package to use.";
    };
  };

  config = mkIf cfg.enable {
    users.groups.fluent-bit.members = [ "fluent-bit" ];
    users.users.fluent-bit = {
      createHome = true;
      isSystemUser = true;
      group = "fluent-bit";
    };

    environment.etc."fluent-bit/parsers.conf".text = cfg.parsers;

    systemd.services.fluent-bit = {
      enable = true;
      serviceConfig = {
        User = "fluent-bit"; 
        Group = "fluent-bit";
        WorkingDirectroy = "/etc/fluent-bit";
        ExecStart = "${cfg.package}/bin/fluent-bit -c ${pkgs.writeText "fluent.conf" cfg.config}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      };
    };
  };
}
