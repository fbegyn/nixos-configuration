# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, modulesPath, ... }:

{
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
    hostName = "infra-01"; # After the Greek titan of dawn
    hostId = "4bd898e6";
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
      ];
    };
  };
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';
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
      "130-mgmt" = {
        netdevConfig = { Kind = "vlan"; Name = "mgmt"; };
        vlanConfig.Id = 130;
      };
    };
    networks = {
      "30-veth0" = {
        matchConfig.Name = "veth0";
        # address = [ "10.5.1.102/24" ];
        # routes = [ { Gateway = "10.5.1.5"; } ];
        vlan = [
          "mgmt"
        ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "carrier";
      };
      # "10-tailscale0" = {
      #   matchConfig.Name = "tailscale*";
      #   linkConfig = {
	  #       Unmanaged = "yes";
      #     RequiredForOnline = "no";
      #   };
      # };
      "130-mgmt" = {
        matchConfig.Name = "mgmt";
        address = [ "10.5.30.101/24" ];
        routes = [ { Gateway = "10.5.30.5"; } ];
        networkConfig.DHCP = "ipv6";
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

  systemd.services.zfs-mount.enable = false;
  systemd.services.zfs-share.enable = false;
  systemd.services.zfs-zed.enable = false;

  age.secrets = {
    "secrets/passwords/mqtt/hass".file = ../../secrets/passwords/mqtt/hass.age;
    "secrets/passwords/mqtt/shelly".file = ../../secrets/passwords/mqtt/shelly.age;
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # VPN settings
  services.tailscale = {
    enable = false;
    useRoutingFeatures = "server";
  };

  # Web/ingress
  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.30.101" ];
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.prometheus = {
    enable = true;
    package = pkgs.unstable.prometheus;
    enableReload = true;
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

  services.mosquitto = {
    enable = true;
    listeners = [
      {
        address = "10.5.90.101";
        users.shelly.passwordFile = config.age.secrets."secrets/passwords/mqtt/shelly".path;
        users.hass.passwordFile = config.age.secrets."secrets/passwords/mqtt/hass".path;
      }
    ];
  };

  # tailfire for tailnet service discovery
  systemd.services."tailfire" = {
    enable = false;
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
    useACMEHost = "infra-01.dcf.begyn.be";

    locations."/" = {
      proxyPass = "http://unix:/run/gitea/gitea.sock";
      proxyWebsockets = true;
    };
    extraConfig = ''
      client_max_body_size 2M;
    '';
  };

  # system packagesystem
  environment.systemPackages = with pkgs; [
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

  # monitoring applications
  ## exporters
  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];
  services.prometheus.exporters.nginx.enable = true;
  services.prometheus.exporters.nginxlog.enable = true;

  home-manager.users.francis = {
    imports = [
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/bash.nix
    ];
  };
  system.stateVersion = "24.11"; # Did you read the comment?
}
