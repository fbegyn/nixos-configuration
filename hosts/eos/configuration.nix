# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
    hosts = import ../../secrets/hosts.nix;
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./acme.nix

    ../../common
    ../../common/network-tools.nix
    ../../common/bluetooth.nix

    ../../users

    # services
    ../../services/coredns
    ../../services/prometheus
    ../../services/tailscale.nix
    ../../services/postgres
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  # disable the laptop lid switch
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';

  environment.systemPackages = with pkgs.unstable; [
    dbus-broker
    nodejs
  ];

  networking = {
    hostName = "eos"; # After the Greek titan of dawn
    hostId = "4bd898f1";
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    defaultGateway = {
      address = "10.5.1.1";
      interface = "eno1";
    };
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    interfaces = {
      eno1.ipv4.addresses = [{ address = "10.5.1.10"; prefixLength = 24; }];
      lan20.ipv4.addresses = [{ address = "10.5.20.10"; prefixLength = 32; }];
      mgmt.ipv4.addresses = [{ address = "10.5.30.10"; prefixLength = 32; }];
      iot.ipv4.addresses = [{ address = "10.5.90.10"; prefixLength = 32; }];
      guests.ipv4.addresses = [{ address = "10.5.100.10"; prefixLength = 32; }];
    };
    vlans = {
      lan20 = { id = 20; interface = "eno1"; };
      mgmt = { id = 30; interface = "eno1"; };
      iot = { id = 90; interface = "eno1"; };
      guests = { id = 100; interface = "eno1"; };
    };
    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS
        3000 # Grafana
        9090 # Prometheus
        8123 # HASS
        28080
      ];
      allowedUDPPorts = [
        5514
        3478
      ];
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # VPN settings
  fbegyn.services.tailscale = {
    enable = true;
    routingFeature = "server";
    autoprovision = {
      enable = true;
      key = "${hosts.tailscale.tempkey}";
      options = [
        "--advertise-routes=${hosts.eos.tailscale.routes}"
        "--advertise-exit-node"
        "--advertise-tags=tag:prod,tag:dcf,tag:hass"
      ];
    };
  };

  # Web/ingress
  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.1.10" ];
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "hass.dcf.begyn.be" = {
        forceSSL = true;
        useACMEHost = "dcf.begyn.be";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8123/";
          proxyWebsockets = true;
        };
      };
      "unifi.svc.begyn.be" = {
        forceSSL = true;
        useACMEHost = "svc-02.begyn.be";
        locations."/" = {
          proxyPass = "https://127.0.0.1:8443/";
          proxyWebsockets = true;
        };
      };
    };
  };
  services.oauth2_proxy = {
    enable = true;
    email.addresses = ''
      francis.begyn@gmail.com
    '';
    nginx.virtualHosts = [
      "news.francis.begyn.be"
    ];
    google = {
      serviceAccountJSON =
        "${hosts.eos.oauth2_proxy.google.serviceAccountJSON}";
    };
    clientID = "${hosts.eos.oauth2_proxy.clientID}";
    keyFile = "${hosts.eos.oauth2_proxy.keyFile}";
    cookie = {
      secret = "${hosts.eos.oauth2_proxy.cookie.secret}";
      expire = "12h0m0s";
    };
  };

  # containers
  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      hass = {
        volumes = [
          "/home/francis/hass:/config"
          "/run/dbus:/run/dbus:ro"
        ];
        environment.TZ = "Europe/Brussels";
        image = "ghcr.io/home-assistant/home-assistant:2023.10";
        extraOptions = [
          "--network=host"
        ];
      };
      eufy-ws-addon = {
        environment = {
          TZ = "Europe/Brussels";
          COUNTRY = "BE";
          PASSWORD = "${hosts.eos.eufy.wsAddon.password}";
          USERNAME = "${hosts.eos.eufy.wsAddon.username}";
          TRUSTED_DEVICE_NAME = "eos";
        };
        image = "bropat/eufy-security-ws:1.6.4";
        ports = [
          "13000:3000"
        ];
      };
      eufy-rtsp-addon = {
        environment = {
          TZ = "Europe/Brussels";
          COUNTRY = "BE";
        };
        image = "bluenviron/mediamtx:1.2.0";
        ports = [
          "1935:1935"
          "8554:8554"
        ];
      };
      # dmarc-report = {
      #   environment = {
      #     TZ = "Europe/Brussels";
      #     COUNTRY = "BE";
      #     REPORT_DB_HOST = "10.88.0.1";
      #     REPORT_DB_PORT = "5432";
      #     REPORT_DB_TYPE = "Pg";
      #     REPORT_DB_NAME = "${hosts.eos.db.dmarc_report.name}";
      #     REPORT_DB_USER = "${hosts.eos.db.dmarc_report.user}";
      #     REPORT_DB_PASS = "${hosts.eos.db.dmarc_report.pass}";
      #     PARSER_IMAP_SERVER = "${hosts.mail.dmarc.hostname}";
      #     PARSER_IMAP_PORT = "993";
      #     PARSER_IMAP_SSL = "1";
      #     PARSER_IMAP_TLS = "0";
      #     PARSER_IMAP_USER = "${hosts.mail.dmarc.mail}";
      #     PARSER_IMAP_PASS = "${hosts.mail.dmarc.pass}";
      #     PARSER_IMAP_READ_FOLDER = "Inbox";
      #     PARSER_IMAP_MOVE_FOLDER = "processed";
      #     PARSER_IMAP_MOVE_FOLDER_ERR = "error";
      #   };
      #   image = "gutmensch/dmarc-report:1.4.1";
      #   ports = [
      #     "21080:80"
      #     "28080:8080"
      #   ];
      # };
    };
  };

  # network services
  services.coredns.enable = true;
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "10.5.90.10";
        users.shelly.password = "${hosts.eos.hass.mqtt.shelly.password}";
        users.hass.password = "${hosts.eos.hass.mqtt.hass.password}";
      }
    ];
  };

  # storage and databases
  services.consul = {
    package = pkgs.unstable.consul;
    enable = true;
    webUi = true;
    interface = {
      bind = "eno1";
    };
    extraConfig = {
      server = true;
      bootstrap_expect = 1;
      datacenter = "app-01";
      bind_addr = "10.5.1.10";
      client_addr = "10.5.1.10";
      enable_script_checks = true;
    };
  };
  services.minio = {
    enable = false;
    rootCredentialsFile = "${hosts.eos.minio.rootCredentialsFile}";
    package = pkgs.unstable.minio;
    region = "eu-west-1";
  };
  services.postgresql = {
    enable = true;
    ensureDatabases = [
      "nextcloud"
      hosts.eos.db.dmarc_report.name
    ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
      {
        name = hosts.eos.db.dmarc_report.user;
        ensurePermissions."DATABASE ${hosts.eos.db.dmarc_report.name}" = "ALL PRIVILEGES";
      }
    ];
    enableTCPIP = true;
    authentication = ''
      local all all trust
      host all all 0.0.0.0/0 md5
      host all all 10.88.0.1/24 md5
    '';
  };
  services.postgresqlBackup = {
    enable = true;
    databases = [ "mastodon" ];
  };


  # monitoring applications
  services.grafana = {
    enable = true;
    settings.server = {
      http_port = 3000;
      http_addr = "";
      protocol = "http";
    };
    dataDir = "/var/lib/grafana";
    package = pkgs.unstable.grafana;
  };
  francis.services.prometheus.retention.time = "365d";
  francis.services.prometheus.retention.size = "32GB";
  services.prometheus = {
    enable = true;
    extraFlags = [
      "--storage.tsdb.allow-overlapping-blocks"
    ];
    ruleFiles = [
      ../../services/prometheus/rules/eos/node.rules
    ];
    alertmanager.enable = true;
    alertmanager.configuration.route = {
      group_by = ["alertname"];
      group_wait = "10s";
      group_interval = "10s";
      repeat_interval = "1h";
      receiver = "default";
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
    scrapeConfigs = [
      {
        job_name = "node-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "eos:9100"
              "mail-01:9100"
              "hosting-01:9100"
            ];
        }];
      }
      {
        job_name = "website";
        scheme = "https";
        static_configs = [{
            targets = [
              "francis.begyn.be"
            ];
        }];
      }
      {
        job_name = "coredns";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.10:9153"
            ];
        }];
      }
      {
        job_name = "home-assistant";
        scheme = "http";
        metrics_path = "/api/prometheus";
        bearer_token = "${hosts.eos.prometheus.hass.token}";
        static_configs = [{
            targets = [
              "10.5.1.10:8123"
            ];
        }];
      }
    ];
  };
  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 9080;
        grpc_listen_port = 0;
      };
      client.url = "http://localhost:3100/loki/api/v1/push";
      scrape_configs = [{
        job_name = "var-logs";
        static_configs = [
          {
            targets = [ "localhost" ];
            labels = {
              job = "varlogs";
              host = "eos";
              __path__ = "/var/log/*.log";
            };
          }
        ];
      }
      {
        job_name = "journald";
        journal = {
          max_age = "12h";
          labels = {
            job = "journald";
            host = "eos";
          };
        };
        relabel_configs = [{
          source_labels = [ "__journal__systemd_unit" ];
          target_label = "unit";
        }];
      }];
    };
  };
  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server.http_listen_port = 3100;
      ingester = {
        lifecycler = {
          address = "127.0.0.1";
          ring = {
            kvstore.store = "inmemory";
            replication_factor = 1;
          };
          final_sleep = "0s";
        };
        chunk_idle_period = "5m";
        chunk_retain_period = "30s";
      };
      schema_config.configs = [{
        from = "2020-05-15";
        store = "boltdb";
        object_store = "filesystem";
        schema = "v11";
        index = {
          prefix = "index_";
          period = "168h";
        };
      }];
      storage_config = {
        boltdb.directory = "/tmp/loki/index";
        filesystem.directory = "/tmp/loki/chunks";
      };
      limits_config = {
        enforce_metric_name = false;
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
      };
    };
  };


  # Web application/services
  ## Nextcloud
  services.nextcloud = {
    enable = false;
    package = pkgs.unstable.nextcloud25;
    enableBrokenCiphersForSSE = false;
    hostName = "docs.begyn.be";
    config = {
      overwriteProtocol = "https";
      defaultPhoneRegion = "BE";

      adminuser = "admin";
      adminpassFile = "${pkgs.writeText "adminpass" "${hosts.eos.nextcloud.adminpass}"}";

      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      dbname = "nextcloud";
      dbpassFile = hosts.eos.nextcloud.dbpassFile;
    };
    https = true;
    autoUpdateApps.enable = true;
    enableImagemagick = true;
  };
  services.nginx.virtualHosts = {
    "docs.begyn.be" = {
      forceSSL = true;
      useACMEHost = "dcf.begyn.be";
    };
  };
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };
  ## tt-rss RSS feed reader
  services.tt-rss = {
    enable = true;
    virtualHost = null;
    selfUrlPath = "https://news.francis.begyn.be";
    database = {
      createLocally = true;
      passwordFile = "/var/lib/tt-rss/db-password";
    };
    email = {
      server = "mail.begyn.be:587";
      login = "bots";
      password = "${hosts.mail.bots.password}";
      security = "tls";
      fromAddress = "bots@begyn.be";
    };
    singleUserMode = true;
    auth = {
      autoCreate = false;
      autoLogin = false;
    };
    logDestination = "syslog";
  };
  services.nginx.virtualHosts."news.francis.begyn.be" = let
    cfg = config.services.tt-rss;
  in {
    root = "${cfg.root}/www";
    forceSSL = true;
    useACMEHost = "francis.dcf.begyn.be";

    locations."/" = {
      index = "index.php";
    };
    locations."^~ /feed-icons" = {
      root = "${cfg.root}";
    };
    locations."~ \\.php$" = {
      extraConfig = ''
        fastcgi_split_path_info ^(.+\.php)(/.+)$;
        fastcgi_pass unix:${config.services.phpfpm.pools.tt-rss.socket};
        fastcgi_index index.php;
      '';
    };
  };
  ## Mastodon
  services.mastodon = {
    enable= true;
    package = pkgs.mastodon;
    localDomain = "social.begyn.be";
    enableUnixSocket = true;
    smtp = {
      host = "mail.begyn.be";
      port = 587;
      authenticate = true;
      user = "bots";
      fromAddress = "social@begyn.be";
      passwordFile = "/var/lib/mastodon/secrets/smtp-password";
      createLocally = false;
    };
    database.createLocally = true;
    redis.createLocally = true;
  };
  users.groups.mastodon.members = [ "nginx" ];
  services.nginx.virtualHosts."social.begyn.be" = let
    cfg = config.services.mastodon;
  in {
    root = "${cfg.package}/public/";
    forceSSL = true;
    useACMEHost = "social.begyn.be";

    locations."/system/".alias = "/var/lib/mastodon/public-system/";
    locations."/" = {
      tryFiles = "$uri @proxy";
    };
    locations."@proxy" = {
      proxyPass = (if cfg.enableUnixSocket then "http://unix:/run/mastodon-web/web.socket" else "http://127.0.0.1:${toString(cfg.webPort)}");
      proxyWebsockets = true;
    };
    locations."/api/v1/streaming/" = {
      proxyPass = (if cfg.enableUnixSocket then "http://unix:/run/mastodon-streaming/streaming.socket" else "http://127.0.0.1:${toString(cfg.streamingPort)}/");
      proxyWebsockets = true;
    };
    extraConfig = ''
      client_max_body_size 40M;
    '';
  };

  home-manager.users.francis.home.stateVersion = "23.05";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}

