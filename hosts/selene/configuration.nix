# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, modulesPath, ... }:

let
    hosts = import ../../secrets/hosts.nix;
in {
  imports = [
    # Include the results of the hardware scan.
    (modulesPath+"/profiles/qemu-guest.nix")
    (modulesPath+"/virtualisation/proxmox-lxc.nix")
    ./hardware-configuration.nix
    ./acme.nix

    ../../common
    ../../common/gpg.nix
    ../../common/network-tools.nix
    ../../common/bluetooth.nix

    ../../users
    ../../users/francis

    # services
    ../../services/tailscale.nix
    ../../services/postgres
  ];

  # no EFI partition on containers
  proxmoxLXC.enable = true;
  boot.isContainer = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "selene"; # After the Greek titan of dawn
    hostId = "4bd898e4";
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
        matchConfig.Name = "eth0";
        address = [ "10.5.1.21/24" ];
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
        address = [ "10.5.20.21/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "130-mgmt" = {
        matchConfig.Name = "mgmt";
        address = [ "10.5.30.21/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "190-iot" = {
        matchConfig.Name = "iot";
        address = [ "10.5.90.21/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
      "1100-guests" = {
        matchConfig.Name = "guests";
        address = [ "10.5.100.21/24" ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

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


  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # VPN settings
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
  };

  # Web/ingress
  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.1.21" ];
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
        address = "10.5.90.21";
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
      bind_addr = "10.5.1.21";
      client_addr = "10.5.1.21";
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

  users.users.root.password = "";

  # monitoring applications
  ## exporters
  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];
  services.prometheus.exporters.blackbox.enable = false;
  services.prometheus.exporters.blackbox.configFile = ./blackbox.yml;
  services.prometheus.exporters.zfs.enable = true;
  services.prometheus.exporters.nginx.enable = true;
  services.prometheus.exporters.nginxlog.enable = true;
  # tailfire for tailnet service discovery
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

  # gitea server
  services.gitea = {
    enable = true;
    settings = {
      service.DISABLE_REGISTRATION = true;
      server = {
        SSH_PORT = 22222;
        PROTOCOL = "http+unix";
        HTTP_ADDR = "/run/gitea/gitea.sock";
        ROOT_URL = "https://gitea.francis.begyn.be";
      };
    };
    database = {
      type = "postgres";
      createDatabase = true;
    };
  };
  services.nginx.virtualHosts."gitea.francis.begyn.be" = {
    forceSSL = true;
    useACMEHost = "francis.dcf.begyn.be";

    locations."/" = {
      proxyPass = "http://unix:/run/gitea/gitea.sock";
      proxyWebsockets = true;
    };
    extraConfig = ''
      client_max_body_size 2M;
    '';
  };

  home-manager.users.francis = {
    imports = [
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/bash.nix
    ];
  };
  system.stateVersion = "24.11"; # Did you read the comment?
}
