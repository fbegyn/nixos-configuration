# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    #<nixos-hardware/dell/xps/13-9360>

    ../../common/base.nix
    ../../common/security.nix
    ../../common/fonts.nix
    ../../common/system.nix
    ../../common/wireguard.nix
    ../../users
    ../../users/francis

    # services
    ../../services/unifi
    ../../services/coredns
    ../../services/consul
    ../../services/vault
    ../../services/tailscale.nix
    ../../services/ddclient
    ../../services/prometheus
    ../../services/grafana
    ../../services/node-exporter
    ../../services/plex.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = pkgs.linuxPackages;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.cleanTmpDir = true;

  nix.autoOptimiseStore = true;
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  # disable the laptop lid switch
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";

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

  networking.wireguard = {
    enable = true;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    pinentryFlavor = "gtk2";
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
  };
  services.hardware.bolt.enable = true;
  nixpkgs.config.allowUnfree = true;

  # tailscale machine specific
  # create a oneshot job to authenticate to Tailscale
  systemd.services.tailscale-autoconnect = let
    hosts = import ../../secrets/hosts.nix;
  in {
    description = "Automatic connection to Tailscale";

    # make sure tailscale is running before trying to connect to tailscale
    after = [ "network-pre.target" "tailscaled.service" ];
    wants = [ "network-pre.target" "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];

    # set this service as a oneshot job
    serviceConfig.Type = "oneshot";

    # have the job run this shell script
    script = with pkgs.unstable; ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${tailscale}/bin/tailscale status -json | ${jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${tailscale}/bin/tailscale up -authkey ${hosts.eos.tailscale.oneoffkey}
    '';
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

  security.acme = {
    acceptTerms = true;
    email = "francis.begyn+certs@gmail.com";
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts.consul =  {
      serverName = "consul.begyn.lan";
      serverAliases = [ "consul" ];
      locations."/" = {
        proxyPass = "http://10.5.1.10:8500";
        extraConfig =
          "proxy_pass_header Authorization;"
          ;
      };
    };
    virtualHosts.grafana =  {
      serverName = "grafana.begyn.lan";
      serverAliases = [ "grafana" ];
      locations."/" = {
        proxyPass = "http://10.5.1.10:3000";
        extraConfig =
          "proxy_pass_header Authorization;"
          ;
      };
    };
    virtualHosts.prometheus =  {
      serverName = "prometheus.begyn.lan";
      serverAliases = [ "prometheus" ];
      locations."/" = {
        proxyPass = "http://10.5.1.10:9090";
        extraConfig =
          "proxy_pass_header Authorization;"
          ;
      };
    };
    virtualHosts.unifi =  {
      serverName = "unifi.begyn.lan";
      serverAliases = [ "unifi" ];
      locations."/" = {
        proxyPass = "https://10.5.1.10:8443";
        extraConfig =
          "proxy_ssl_server_name on;" +
          "proxy_pass_header Authorization;"
          ;
      };
    };
  };

  # serve as repo for mailserver
  users.groups.virtualmail.gid = 2000;
  users.users.virtualmail = {
    createHome = true;
    isSystemUser = true;
    packages = [ pkgs.borgbackup ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICy0YyOZjqBDZeFjnfFnVUoUH5j4SZpPKGQEw3VjtrxS Borg Backup"
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

