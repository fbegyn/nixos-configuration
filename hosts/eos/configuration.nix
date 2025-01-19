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
    ../../common/gpg.nix
    ../../common/network-tools.nix
    ../../common/bluetooth.nix

    ../../users
    ../../users/francis

    # services
    # ../../services/coredns
    ../../services/prometheus
    ../../services/tailscale.nix
    ../../services/postgres
  ];

  services.blocky = {
    enable = true;
    settings = {
      ports.dns = 53;
      ports.tls = 853;
      ports.http = 14000;
      log.format = "json";
      upstreams.groups = {
        default = [
          "https://one.one.one.one/dns-query"
          "1.1.1.1"
          "8.8.8.8"
          "10.5.10.5"
          "10.5.20.5"
          "10.5.30.5"
          "10.5.90.5"
        ];
      };
      bootstrapDns = [
        { upstream = "https://one.one.one.one/dns-query"; ips = [ "1.1.1.1" "8.8.8.8" ]; }
      ];
      blocking = {
        blackLists = {
	        ads = [
	          "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt"
	          "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          ];
	      };
	      clientGroupsBlock = {
	        default = [
	          "ads"
	        ];
	      };
      };
      caching = {
        minTime = "4h";
	      maxTime = "48h";
	      maxItemsCount = 5000;
	      prefetching = true;
	      prefetchMaxItemsCount = 300;
      };
      prometheus.enable = true;
      queryLog = {
        type = "csv";
        target = "/var/tmp";
        logRetentionDays = 5;
        fields = [ "question" "duration" "responseReason" "responseAnswer"];
        flushInterval = "30s";
      };
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" "wasm32-wasi" ];

  systemd.services.zfs-mount.enable = false;

  # disable the laptop lid switch
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "eos"; # After the Greek titan of dawn
    hostId = "4bd898f1";
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS
        3000 # Grafana
        9090 # Prometheus
        19090 # tailfire
        8123 # HASS
        19925 # mealie
        28080
        # prometheus exporter ports
        9115
        9100
        9134
        9113
        9117
      ];
      allowedUDPPorts = [
        5514
        3478
      ];
    };
  };
  systemd.network= {
    enable = true;
    wait-online = {
      enable = true;
      ignoredInterfaces = [
        "tailscale*"
        "tailscale0"
        "veth*"
        "wlp*"
        "wlp3s0"
      ];
    };
    netdevs = {
      "120-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "lan"; };
        vlanConfig.Id = 120;
      };
      "130-mgmt" = {
        netdevConfig = { Kind = "vlan"; Name = "mgmt"; };
        vlanConfig.Id = 130;
      };
      "190-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "iot"; };
        vlanConfig.Id = 190;
      };
      "1100-lan" = {
        netdevConfig = { Kind = "vlan"; Name = "guests"; };
        vlanConfig.Id = 1100;
      };
    };
    networks = {
      "30-eno1" = {
        matchConfig.Name = "eno1";
        address = [ "10.5.1.10/24" ];
        routes = [ { Gateway = "10.5.1.5"; } ];
        vlan = [
          "lan"
          "mgmt"
          "iot"
          "guests"
        ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "carrier";
      };
      "10-tailscale0" = {
        matchConfig.Name = "tailscale*";
        linkConfig = {
	  Unmanaged = "yes";
          RequiredForOnline = "no";
	};
      };
      "120-lan" = {
        matchConfig.Name = "lan";
        address = [ "10.5.20.10/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "130-mgmt" = {
        matchConfig.Name = "mgmt";
        address = [ "10.5.30.10/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "190-iot" = {
        matchConfig.Name = "iot";
        address = [ "10.5.90.10/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "1100-guests" = {
        matchConfig.Name = "guests";
        address = [ "10.5.100.10/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # VPN settings
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    # autoprovision = {
    #   enable = true;
    #   key = "${hosts.tailscale.tempkey}";
    #   options = [
    #     "--advertise-routes=${hosts.eos.tailscale.routes}"
    #     "--advertise-exit-node"
    #     "--advertise-tags=tag:prod,tag:dcf,tag:hass"
    #   ];
    # };
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
      "mealie.dcf.begyn.be" = {
        forceSSL = true;
        useACMEHost = "dcf.begyn.be";
        locations."/" = {
          proxyPass = "http://127.0.0.1:19925/";
          proxyWebsockets = true;
        };
      };
      "hass.dcf.begyn.be" = {
        forceSSL = true;
        useACMEHost = "dcf.begyn.be";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8123/";
          proxyWebsockets = true;
        };
      };
      "aptly.repo.begyn.be" = {
        forceSSL = true;
        useACMEHost = "dcf.begyn.be";
        locations."/" = {
	  root = "/var/www/aptly.repo.begyn.be/public/";
	  extraConfig = ''
	    allow all;
	    sendfile on;
	    sendfile_max_chunk 1m;
	    autoindex on;
	    autoindex_exact_size off;
	    autoindex_format html;
	    autoindex_localtime on;
          '';
        };
      };
      "rpm.repo.begyn.be" = {
        forceSSL = true;
        useACMEHost = "dcf.begyn.be";
        locations."/" = {
	  root = "/var/www/rpm.repo.begyn.be/";
	  extraConfig = ''
	    allow all;
	    sendfile on;
	    sendfile_max_chunk 1m;
	    autoindex on;
	    autoindex_exact_size off;
	    autoindex_format html;
	    autoindex_localtime on;
          '';
        };
      };
    };
  };
  services.oauth2-proxy = {
    enable = true;
    email.addresses = ''
      francis.begyn@gmail.com
    '';
    nginx = {
      domain = "francis.begyn.be";
      virtualHosts = {
        "news.francis.begyn.be" = {
          allowed_emails = [ "francis.begyn@gmail.com" ];
        };
      };
    };
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

  services.paperless = {
    enable = false;
    package = pkgs.unstable.paperless-ngx;
    passwordFile = "${hosts.eos.paperless.passwordPath}";
    address = "10.5.1.10";
    settings = {
      PAPERLESS_ADMIN_USER = "${hosts.eos.paperless.adminUser}";
      PAPERLESS_OCR_USER_ARGS = "{\"invalidate_digital_signatures\": true}";
      PAPERLESS_URL="${hosts.eos.paperless.url}";
      ALLOWED_HOSTS="${hosts.eos.paperless.allowedHosts}";
    };
  };
  services.nginx.virtualHosts."paperless.francis.begyn.be" = {
    forceSSL = true;
    useACMEHost = "francis.dcf.begyn.be";
    locations."/" = {
      proxyPass = "http://10.5.1.10:28981$request_uri";
      proxyWebsockets = true;
    };
  };

  # system packagesystem
  environment.systemPackages = with pkgs; [
    bluez
    aptly
    createrepo_c
  ];

  # containers
  services.dbus = {
    enable = true;
    implementation = "broker";
    packages = [ pkgs.bluez ];
  };
  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
    defaultNetwork.settings = {
      dns_enabled = true;
    };
  };
  virtualisation.oci-containers = {
    backend = "podman";
  };

  # network services
  services.coredns.enable = false;
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
    enable = false;
    package = pkgs.unstable.consul;
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
      hosts.eos.db.dmarc_report.user
    ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensureDBOwnership = true;
      }
      {
        name = hosts.eos.db.dmarc_report.user;
        ensureDBOwnership = true;
      }
    ];
    enableTCPIP = false;
    authentication = ''
      local all all trust
      host all all 0.0.0.0/0 md5
      host all all 10.88.0.1/16 md5
    '';
  };
  services.postgresqlBackup = {
    enable = true;
    databases = [ "mastodon" ];
  };

  # monitoring applications
  ## exporters
  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];
  services.prometheus.exporters.blackbox.enable = true;
  services.prometheus.exporters.blackbox.configFile = ./blackbox.yml;
  services.prometheus.exporters.zfs.enable = true;
  services.prometheus.exporters.nginx.enable = true;
  services.prometheus.exporters.nginxlog.enable = true;
  ## grafana
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
  ## prometheus
  francis.services.prometheus.retention.time = "365d";
  francis.services.prometheus.retention.size = "32GB";
  services.prometheus = {
    enable = true;
    listenAddress = "10.5.1.10";
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
	http_sd_configs = [{
	  url = "http://localhost:19090/prometheus/targets";
	}];
	relabel_configs = [
	  {
            source_labels = ["__meta_tailscale_device_hostname"];
            target_label = "instance";
	  }
	  {
	    source_labels = ["__address__"];
	    target_label = "__address__";
	    replacement = "$1:9100";
	  }
	  {
            source_labels = ["__meta_tailscale_device_tags"];
            action = "keep";
            regex = ".*,tag:node,.*";
            separator = ",";
	  }
	];
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
        job_name = "blocky";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.10:14000"
            ];
        }];
      }
      {
        job_name = "tc-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.20.5:9704"
            ];
        }];
      }
      {
        job_name = "kea-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.20.5:9547"
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
  systemd.services."tailfire" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = ''
        /usr/local/bin/tailfire serve \
          --port 19090 \
          --config.file /etc/tailfire/config.yaml
      '';
      ExecReload = "kill -SIGHUP $MAINPID";
      User = "prometheus";
      Restart = "always";
      ReadOnlyPaths = [ "/etc/tailfire" ];
      RuntimeDirectory = "prometheus";
      RuntimeDirectoryMode = "0700";
      WorkingDirectory = "/var/lib/tailfire";
      DeviceAllow = [ "/dev/null rw" ];
      DevicePolicy = "strict";
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateTmp = true;
      PrivateUsers = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProtectSystem = "full";
      RemoveIPC = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [ "@system-service" "~@privileged" ];
    };
  };

  ## logging
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
      schema_config.configs = [
      {
        from = "2020-05-15";
        store = "boltdb";
        object_store = "filesystem";
        schema = "v11";
        index = {
          prefix = "index_";
          period = "168h";
        };
      }
      {
        from = "2024-07-08";
        store = "tsdb";
        object_store = "filesystem";
        schema = "v13";
        index = {
          prefix = "index_";
          period = "24h";
        };
      }
      ];
      storage_config = {
        boltdb.directory = "/tmp/loki/index";
        filesystem.directory = "/tmp/loki/chunks";
        tsdb_shipper = {
          active_index_directory = "/tmp/loki/tsdb/index";
          cache_location = "/tmp/loki/tsdb/cache";
	};
      };
      compactor = {
        working_directory = "/tmp/loki/tsdb/retention";
        compaction_interval = "10m";
        retention_enabled = true;
        retention_delete_delay = "2h";
        retention_delete_worker_count = 150;
        delete_request_store = "filesystem";
      };
      limits_config = {
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
	allow_structured_metadata = false;
      };
    };
  };


  # Web application/services
  services.nginx.virtualHosts."files.svc.begyn.be" = {
    forceSSL = true;
    useACMEHost = "dcf.begyn.be";
    root = "/var/run/files.svc.begyn.be/files";
    locations."/" = {
      tryFiles = "$uri $uri/ =404";
      basicAuthFile = "/var/run/files.svc.begyn.be/auth";
      extraConfig = ''
        autoindex on;
        sendfile on;
	sendfile_max_chunk 2m;
	tcp_nopush on;
	tcp_nodelay on;
	keepalive_timeout 65;
      '';
    };
  };
  ## Nextcloud
  services.nextcloud = {
    enable = false;
    package = pkgs.unstable.nextcloud25;
    hostName = "docs.begyn.be";
    settings = {
      overwriteprotocol = "https";
      default_phone_region = "BE";
    };
    config = {
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
    enable = true;
    package = pkgs.mastodon;
    localDomain = "social.begyn.be";
    enableUnixSocket = true;
    streamingProcesses = 2;
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

  # arr-suite
  services.transmission = {
    enable = true;
    home = "/var/lib/transmission";
    performanceNetParameters = true;
    openRPCPort = true;
    downloadDirPermissions = "775";
    settings = {
      download-dir = "/storage/downloads/torrents";
      bind-address-ipv4 = "0.0.0.0";
      bind-address-ipv6 = "::";
      rpc-bind-address = "0.0.0.0";
      rpc-whitelist = "127.0.0.1,::1,10.5.20.*,10.5.1.*,10.5.30.*,10.88.*.*";
      rpc-password = "{243ebd27fb1c15ed4f697f12f351d245103ec33azhmve5t0";
      rpc-username = "fbegyn";
      umask = 2;
    };
  };
  services.jellyfin = {
    enable = true;
    dataDir = "/var/lib/jellyfin";
    openFirewall = true;
  };

  home-manager.users.francis.home.stateVersion = "23.05";
  home-manager.users.francis = {
    imports = [
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/bash.nix
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}

