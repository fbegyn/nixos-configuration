# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running 'nixos-help').

{ config, pkgs, modulesPath, ... }:

let
  proxFunc = (import ../../lib/proxmox.nix);
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
  ];

  networking = {
    hostName = "app-01"; # After the Greek titan of dawn
    hostId = "4bd898e7";
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    nameservers = [ "10.5.1.5" "1.1.1.1" "8.8.8.8" ];
    search = [ "lan" "begyn.lan" ];
    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS

        8123 # hass
        1883 # mqqt
        5683 # CoIoT
      ];
    };
  };
  services.resolved.extraConfig = ''
    DNSStubListener=no
  '';
  systemd.network.networks = proxFunc.mkContainerNetworks "103";

  # VPN settings
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_17;
    enableTCPIP = true;
    port = 5432;
    authentication = pkgs.lib.mkOverride 10 ''
      local  all  all  trust
      host   all  all  127.0.0.1/32 scram-sha-256
      host   all  all  10.89.0.1/24 scram-sha-256
    '';
    ensureDatabases = [
      "gitea"
      "forgejo"
      "hass"
    ];
    ensureUsers = [
      {
        name = "hass";
        ensureDBOwnership = true;
      }
    ];
  };

  # Web/ingress
  services.nginx = {
    enable = true;
    defaultListenAddresses = [ "10.5.1.103" ];
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
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
