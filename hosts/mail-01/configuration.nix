# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
    hosts = import ../../secrets/hosts.nix;
    vars = hosts.mail-01;
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../../common
    ../../common/security.nix

    ../../users
    ../../users/francis

    ./acme.nix
    ../../services/tailscale.nix

    # simple mail server
    (builtins.fetchTarball {
      # Pick a commit from the branch you are interested in
      url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/8b287056215cac91438a671054e7eb2c932ab21a/nixos-mailserver-8b287056215cac91438a671054e7eb2c932ab21a.tar.gz";
      # And set its hash
      sha256 = "1h1x2498j5ki6pajsbgwq664j9isihzgjsg87fgqdzyizrxn0mai";
    })
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  boot.initrd.supportedFilesystems = ["zfs"]; # boot from zfs
  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "mail-01"; # Define your hostname.
  networking.hostId = vars.hostId;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;
  networking.interfaces.ens3.ipv6 = {
    addresses = [
      {
        address = "2a01:4f8:1c1c:af60::1";
        prefixLength = 64;
      }
    ];
  };
  networking.defaultGateway6 = {
    address = "fe80::1";
    interface = "ens3";
  };
  networking.firewall.interfaces = {
    "tailscale0" = {
      allowedTCPPorts = [ 22 ];
    };
  };

  # networking.firewall.enable = false;
  # networking.nftables = {
  #   enable = true;
  #   rulesetFile = ./nftables.rules;
  # };

  # tailscale machine specific
  thecy.services.tailscale = {
    enable = true;
    autoprovision = {
      enable = true;
      key = "${vars.tailscale.oneoffkey}";
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  # Fail2ban
  services.fail2ban = {
    enable = true;
    jails = {
      dovecot = ''
        enabled  = true
        port     = pop3,pop3s,imap,imap
        filter   = dovecot
        maxretry = 3
      '';
      postfix = ''
        enabled  = true
        port     = smtp, ssmtp
        filter   = postfix
        maxretry = 5
      '';
    };
  };

  mailserver = {
    enable = true;
    fqdn = vars.mailserver.fqdn;
    domains = vars.mailserver.domains;
    loginAccounts = vars.mailserver.accounts;
    certificateScheme = 3;
    virusScanning = false;
    monitoring = {
      enable = false;
      alertAddress = vars.mailserver.alertAddress;
    };
  };

  # mailserver backups
  # borgbase
  services.borgbackup.jobs."borgbase" = {
    paths = [
      "/var/vmail"
      "/var/dkim"
    ];
    repo = vars.mailserver.backups.borgbase.repo;
    encryption = {
      mode = "repokey-blake2";
      passCommand = vars.mailserver.backups.borgbase.key;
    };
    environment.BORG_RSH = vars.mailserver.backups.borgbase.ssh;
    compression = "auto,lzma";
    startAt = "*:0/20";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

