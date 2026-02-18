# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running 'nixos-help').

{ config, pkgs, modulesPath, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./acme.nix

    ../../lib/proxmox-lxc.nix

    ../../common
    ../../common/gpg.nix
    ../../common/network-tools.nix
    ../../common/bluetooth.nix

    ../../users
    ../../users/francis

    # services
    ../../services/postgres
    ../../services/blocky
  ];

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
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';
  systemd.network= {
    wait-online.ignoredInterfaces = [ "veth*" ];
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

  age.secrets = {
    "secrets/passwords/mqtt/hass".file = ../../secrets/passwords/mqtt/hass.age;
    "secrets/passwords/mqtt/shelly".file = ../../secrets/passwords/mqtt/shelly.age;
  };

  services.blocky.settings.upstreams.groups.default = [
    "https://one.one.one.one/dns-query"
    "1.1.1.1"
    "8.8.8.8"
    "10.5.10.5"
    "10.5.20.5"
    "10.5.30.5"
    "10.5.90.5"
  ];

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
        useACMEHost = "selene.dcf.begyn.be";
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
        users.shelly.passwordFile = config.age.secrets."secrets/passwords/mqtt/shelly".path;
        users.hass.passwordFile = config.age.secrets."secrets/passwords/mqtt/hass".path;
      }
    ];
  };

  # monitoring applications
  ## exporters
  services.prometheus.exporters.nginx.enable = true;
  services.prometheus.exporters.nginxlog.enable = true;

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
    useACMEHost = "selene.dcf.begyn.be";

    locations."/" = {
      proxyPass = "http://unix:/run/gitea/gitea.sock";
      proxyWebsockets = true;
    };
    extraConfig = ''
      client_max_body_size 2M;
    '';
  };

  system.stateVersion = "24.11"; # Did you read the comment?
}
