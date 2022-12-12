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
    ../../common/rtlsdr.nix

    ../../users
    ../../users/francis

    # services
    ../../services/coredns
    ../../services/ddclient
    ../../services/prometheus
    ../../services/grafana
    ../../services/node-exporter
    ../../services/tailscale.nix
    ../../services/plex.nix
    ../../services/postgres
    ../../services/consul
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = pkgs.linuxPackages_latest;
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

  virtualisation.docker.enable = true;
  services.jupyterhub = {
    enable = false;
    jupyterlabEnv = pkgs.python3.withPackages (p: with p; [
      jupyterhub
      jupyterlab
      dockerspawner
    ]);
    jupyterhubEnv = pkgs.python3.withPackages (p: with p; [
      jupyterhub
      jupyterlab
      dockerspawner
    ]);
    spawner = "dockerspawner.DockerSpawner";
    extraConfig = ''
      c.JupyterHub.hub_ip = '10.5.1.10'
      c.Authenticator.allowed_users = { "francis" }
      c.Authenticator.admin_users = { "francis" }

      docker_notebook_dir = '/home/jovyan/work'
      c.DockerSpawner.image = 'jupyter/datascience-notebook:845d8ab3cd9d'
      c.DockerSpawner.hub_connect_ip = '10.5.1.10'
      c.DockerSpawner.notebook_dir = docker_notebook_dir
      c.DockerSpawner.volumes = { '/home/{username}/jupyterhub': docker_notebook_dir }
      c.DockerSpawner.remove_containers = True
      c.DockerSpawner.remove = True
    '';
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      hass = {
        volumes = [
          "/home/francis/hass:/config"
          "/run/dbus:/run/dbus:ro"
        ];
        environment.TZ = "Europe/Brussels";
        image = "ghcr.io/home-assistant/home-assistant:2022.12";
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
        image = "bropat/eufy-security-ws:1.2.0";
        ports = [
          "13000:3000"
        ];
      };
      eufy-rtsp-addon = {
        environment = {
          TZ = "Europe/Brussels";
          COUNTRY = "BE";
        };
        image = "aler9/rtsp-simple-server:v0.20.4";
        ports = [
          "1935:1935"
          "8554:8554"
        ];
      };
    };
  };

  environment.systemPackages = with pkgs.unstable; [
    dbus-broker
  ];

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

  networking = {
    hostName = "eos"; # After the Greek titan of dawn
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    defaultGateway = {
      address = "10.5.1.1";
      interface = "enp57s0u1";
    };
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    interfaces = {
      enp57s0u1.ipv4.addresses = [{ address = "10.5.1.10"; prefixLength = 24; }];
      lan20.ipv4.addresses = [{ address = "10.5.20.10"; prefixLength = 32; }];
      mgmt.ipv4.addresses = [{ address = "10.5.30.10"; prefixLength = 32; }];
      iot.ipv4.addresses = [{ address = "10.5.90.10"; prefixLength = 32; }];
      guests.ipv4.addresses = [{ address = "10.5.100.10"; prefixLength = 32; }];
    };
    vlans = {
      lan20 = { id = 20; interface = "enp57s0u1"; };
      mgmt = { id = 30; interface = "enp57s0u1"; };
      iot = { id = 90; interface = "enp57s0u1"; };
      guests = { id = 100; interface = "enp57s0u1"; };
    };
    firewall = {
      enable = false;
      allowedTCPPorts = [ 80 443 3000 9090 8443 8123 22 ];
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # tailscale machine specific
  thecy.services.tailscale = {
    enable = true;
    autoprovision = {
      enable = true;
      key = "${hosts.eos.tailscale.oneoffkey}";
    };
  };

  services.ddclient = {
    zone = "begyn.be";
    domains = [
      "dcf.begyn.be"
    ];
  };

  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.1.10" ];
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
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

  services.avahi = {
    enable = true;
    reflector = true;
    interfaces = [
      "enp57s0u1"
      "lan0"
      "iot"
    ];
  };

  services.nginx.virtualHosts = {
    "hass.dcf.begyn.be" = {
      forceSSL = true;
      useACMEHost = "dcf.begyn.be";
      locations."/" = {
        proxyPass = "http://127.0.0.1:8123/";
        proxyWebsockets = true;
      };
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [
      "nextcloud"
    ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
    ];
  };
  services.postgresqlBackup = {
    enable = true;
    databases = [ "mastodon" ];
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.unstable.nextcloud25;
    enableBrokenCiphersForSSE = false;
    hostName = "docs.begyn.be";
    config = {
      overwriteProtocol = "https";

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

  services.consul = {
    interface = {
      bind = "enp57s0u1";
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

  services.redis.servers = {
    default = {
      enable = true;
      bind = "0.0.0.0";
      openFirewall = true;
      requirePassFile = "/var/lib/redis/pass";
    };
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

  services.mastodon = {
    enable= true;
    package = pkgs.unstable.mastodon;
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

  services.prometheus= {
    ruleFiles = [
      ../../services/prometheus/rules/eos/node.rules
    ];
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

  services.minio = {
    enable = true;
    rootCredentialsFile = "${hosts.eos.minio.rootCredentialsFile}";
    package = pkgs.unstable.minio;
    region = "eu-west-1";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}

