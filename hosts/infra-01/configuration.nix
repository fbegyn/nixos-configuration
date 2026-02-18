# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running 'nixos-help').

{ config, pkgs, modulesPath, ... }:

let
  proxFunc = import ../../lib/proxmox.nix;
in {
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
  systemd.network.networks = proxFunc.mkContainerNetworks "101";

  age.secrets = {
    "secrets/passwords/mqtt/hass".file = ../../secrets/passwords/mqtt/hass.age;
    "secrets/passwords/mqtt/shelly".file = ../../secrets/passwords/mqtt/shelly.age;
  };

  # VPN settings
  services.tailscale = {
    enable = false;
    useRoutingFeatures = "server";
  };

  # Web/ingress
  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.1.101" ];
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

  services.blocky.settings.upstreams.groups.default = [
    "https://one.one.one.one/dns-query"
    "1.1.1.1"
    "8.8.8.8"
    "10.5.10.5"
  ];

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
  services.nginx.virtualHosts."git.svc.begyn.be" = {
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
  services.prometheus.exporters.nginx.enable = true;
  services.prometheus.exporters.nginxlog.enable = true;

  system.stateVersion = "25.11"; # Did you read the comment?
}
