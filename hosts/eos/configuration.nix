# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../../common
    ../../common/security.nix

    ../../users
    ../../users/francis

    # services
    ../../services/unifi
    ../../services/coredns
    ../../services/ddclient
    ../../services/prometheus
    ../../services/grafana
    ../../services/node-exporter
    ../../services/tailscale.nix
    ../../services/plex.nix
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
    interfaces.enp57s0u1 = {
      ipv4.addresses = [
        { address  = "10.5.1.10"; prefixLength = 24; }
      ];
    };
    firewall.enable = false;
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
  thecy.services.tailscale = let
    hosts = import ../../secrets/hosts.nix;
  in {
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

  services.prometheus= {
    scrapeConfigs = [
      {
        job_name = "node-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.1:9100"
              "10.5.1.10:9100"
            ];
        }];
      }
      {
        job_name = "unifi";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.10:9130"
            ];
        }];
      }
      {
        job_name = "tc-exporter";
        scheme = "http";
        static_configs = [{
            targets = [
              "10.5.1.1:9704"
            ];
        }];
      }
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

