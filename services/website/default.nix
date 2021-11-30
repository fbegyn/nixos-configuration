{ config, pkgs, lib, ...}:


let
  cfg = config.thecy.services.website;
in
with lib; {
  options.thecy.services.website = {
    enable = mkEnableOption "enables fbegyn's personal website server";

    domain = mkOption {
      type = types.str;
      default = "francis.begyn.be";
      example = "francis.begyn.be";
      description = "The domain NGINX should use.";
    };

    aliases = mkOption {
      type = types.list;
      default = [];
      example = [ "francis.begyn.eu" ];
      description = "The aliases NGINX should use.";
    };

    port = mkOption {
      type = types.int;
      default = 3114;
      example = 3114;
      description = "The port number for the website server";
    };
  };

  config = mkIf cfg.enable {
    users.users.thecywebsite = {
      createHome = true;
      isSystemUser = true;
      group = "thecy";
      home = "/srv/thecy/website";
      description = "francis.begyn.be";
    };

    systemd.services.website = {
      enable = true;

      serviceConfig = {
        Environment="SERVER_PORT=${toString cfg.port}";
        User = "thecywebsite";
        Group = "thecy";
        WorkingDirectory = "/srv/thecy/website";
      };

      script = let
        server = pkgs.fbegyn.website;
      in ''
        exec ${server}/bin/server
      '';

      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
    };

    # francis.begyn.be website/blog
    services.nginx.virtualHosts.thecywebsite = {
      forceSSL = true;
      serverName = "${cfg.domain}";
      serverAliases = ${cfg.aliases};
      useACMEHost = "francis.begyn.be";
      root = "/var/www/francis.begyn.be";
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.port}";
        extraConfig = ''
          add_header Permissions-Policy interest-cohort=();
        '';
      };
    };
  };
}
