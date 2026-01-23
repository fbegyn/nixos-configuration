# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, modulesPath, ... }:

let
  proxFunc = import ../../lib/proxmox.nix;
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
    hostName = "proxy-01"; # After the Greek titan of dawn
    hostId = "4bd898e5";
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
    firewall = {
      enable = true;
      interfaces = {
        eth0 = {
          allowedTCPPorts = [
            2022
            80
            443
            8404
            8405
          ];
        };
        mgmt = {
          allowedTCPPorts = [
            22
          ];
        };
      };
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
        "eth*"
        "wlp*"
        "wlp3s0"
      ];
    };
    # netdevs = proxFunc.mkContainerNetdevs;
    networks = proxFunc.mkContainerNetworks "102";
  };

  systemd.services.zfs-mount.enable = false;
  systemd.services.zfs-share.enable = false;
  systemd.services.zfs-zed.enable = false;

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
    defaultListenAddresses = [ "10.5.1.102" ];
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  services.haproxy = {
    enable = true;
    config = ''
      defaults
        mode http
        balance roundrobin

      frontend stats
        bind 10.5.1.102:8404
        stats enable
        stats uri /stats
        stats refresh 10s
        stats admin if TRUE

      frontend metrics
        bind 10.5.1.102:8405
        mode http
        http-request use-service prometheus-exporter if { path /metrics }
        no log

      frontend http
        bind 10.5.1.102:80
        default_backend app

      frontend https
        bind 10.5.1.102:443
        acl git_https_traffic hdr(host) -i git.begyn.be
        use_backend infra if git_https_traffic
        default_backend app

      frontend ssh
        bind 10.5.1.102:2022
        acl git_ssh_traffic hdr(host) -i git.begyn.be
        use_backend infra if git_ssh_traffic

      backend infra
        server infra-01 10.5.1.101

      backend app
        server app-01 10.5.1.103
    '';
  };

  # system packagesystem
  environment.systemPackages = with pkgs; [
    aptly
    createrepo_c
  ];

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
  system.stateVersion = "25.11"; # Did you read the comment?
}
