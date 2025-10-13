# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ../../common
    ../../common/gpg.nix
    ../../common/network-tools.nix

    ../../users
    ../../users/francis
  ];

  # no EFI partition on containers
  proxmoxLXC.enable = true;
  boot.isContainer = true;
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  systemd.services.zfs-mount.enable = false;

  networking = {
    wireless.enable = false;
    useDHCP = false;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS
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
      "30-eth0" = {
        matchConfig.Name = "eth0";
        address = [ "10.5.1.127/24" ];
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
    };
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };

  # system packagesystem
  environment.systemPackages = with pkgs; [
    vim
    neovim
    wget
  ];

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

  users.users.root.password = "foobar";
  home-manager.users.francis = {
    imports = [
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/bash.nix
    ];
  };
  system.stateVersion = "24.11"; # Did you read the comment?
}
