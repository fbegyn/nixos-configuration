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

    ../../common
    ../../common/gpg.nix
    ../../common/network-tools.nix
    ../../common/bluetooth.nix

    ../../users
    ../../users/francis

    # services
  ];

  # no EFI partition on containers
  proxmoxLXC.enable = true;
  boot.isContainer = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "nix-builder-01"; # After the Greek titan of dawn
    hostId = "4bd89810";
    wireless.enable = false;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    firewall = {
      enable = false;
      allowedTCPPorts = [
        22 # ssh
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
    networks = {
      "30-eno1" = {
        matchConfig.Name = "eth0";
        address = [ "10.5.1.22/24" ];
        routes = [ { Gateway = "10.5.1.5"; } ];
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
    };
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
    enable = true;
    useRoutingFeatures = "server";
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

  home-manager.users.francis = {
    imports = [
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/bash.nix
    ];
  };
  system.stateVersion = "24.11"; # Did you read the comment?
}
