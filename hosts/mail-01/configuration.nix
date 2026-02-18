# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
    vars = import ../../secrets/services/mail.nix { inherit config; };
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./acme.nix
    ../../users
  ];

  boot.initrd.supportedFilesystems = ["zfs"]; # boot from zfs
  boot.supportedFilesystems = [ "zfs" ];

  networking.hostId = "21c69462";

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
      allowedTCPPorts = [ 22 9100 ];
    };
    "ens3" = {
      allowedTCPPorts = [ 25 80 143 443 465 587 993 ];
    };
  };
  networking.firewall.package = pkgs.iptables-nftables-compat;

  # networking.firewall.enable = false;
  # networking.nftables = {
  #   enable = true;
  #   rulesetFile = ./nftables.rules;
  # };

  services.prometheus.exporters.node.enable = true;

  age.secrets = {
    "secrets/passwords/mail/francis" = {
      file = ../../secrets/passwords/mail/francis.age;
      mode = "0644";
      owner = "virtualMail";
      group = "virtualMail";
    };
    "secrets/passwords/mail/marc" = {
      file = ../../secrets/passwords/mail/marc.age;
      mode = "0644";
      owner = "virtualMail";
      group = "virtualMail";
    };
    "secrets/passwords/mail/dmarc" = {
      file = ../../secrets/passwords/mail/dmarc.age;
      mode = "0644";
      owner = "virtualMail";
      group = "virtualMail";
    };
    "secrets/passwords/mail/bots" = {
      file = ../../secrets/passwords/mail/bots.age;
      mode = "0644";
      owner = "virtualMail";
      group = "virtualMail";
    };
    "secrets/passwords/mail/robot" = {
      file = ../../secrets/passwords/mail/robot.age;
      mode = "0644";
      owner = "virtualMail";
      group = "virtualMail";
    };

    "secrets/api/cf".file = ../../secrets/api/cf.age;
    "secrets/api/tailscale-temp".file = ../../secrets/api/tailscale-temp.age;

    "secrets/data/borgbase/key".file = ../../secrets/data/borgbase/key.age;
    "secrets/data/borgbase/ssh".file = ../../secrets/data/borgbase/ssh.age;
  };

  services.tailscale = {
    enable = true;
    authKeyFile = config.age.secrets."secrets/api/tailscale-temp".path;
    extraUpFlags = [
      "--operator=francis"
      "--advertise-tags=tag:prod,tag:hetzner,tag:cloud"
    ];
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  # Fail2ban
  services.fail2ban = {
    enable = true;
    package = pkgs.fail2ban;
    packageFirewall = config.networking.firewall.package;
    extraPackages = [ pkgs.ipset ];
    maxretry = 5;
    bantime = "15m";
    ignoreIP = [
      "213.119.124.156"
      "100.88.113.9"
      "109.236.137.143"
    ];
    jails = {
      postfix-bruteforce = ''
        enabled  = true
        filter   = postfix-bruteforce
        findtime = 600
        maxretry = 3
        bantime  = 12h
      '';
      postfix-ssl-error = ''
        enabled  = true
        filter   = postfix-ssl-error
        findtime = 600
        maxretry = 3
        bantime  = 12h
      '';
      postfix-improper-command = ''
        enabled  = true
        filter   = postfix-improper-command
        findtime = 600
        maxretry = 2
        bantime  = 12h
      '';
    };
  };
  environment.etc = {
    "fail2ban/filter.d/postfix-bruteforce.conf".text = ''
      [Definition]
      failregex= warning: [\w\.\-]+\[<HOST>\]: SASL LOGIN authentication failed.*$
      journalmatch = _SYSTEMD_UNIT=postfix.service
    '';
    "fail2ban/filter.d/postfix-ssl-error.conf".text = ''
      [Definition]
      failregex= SSL_accept error from unknown\[<HOST>\]: *
      journalmatch = _SYSTEMD_UNIT=postfix.service
    '';
    "fail2ban/filter.d/postfix-improper-command.conf".text = ''
      [Definition]
      failregex= improper command pipelining after CONNECT from unknown\[<HOST>\]: *
      journalmatch = _SYSTEMD_UNIT=postfix.service
    '';
  };

  mailserver = {
    enable = true;
    fqdn = vars.fqdn;
    domains = vars.domains;
    loginAccounts = vars.accounts;
    certificateScheme = "acme-nginx";
    certificateDomains = vars.certificateDomains;
    virusScanning = false;
    monitoring = {
      enable = false;
      alertAddress = vars.alertAddress;
    };
  };
  services.nginx.virtualHosts."autoconfig.begyn.be" = {
    useACMEHost = "mx-01.begyn.be";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:11234";
    };
  };
  services.go-autoconfig = {
    enable = true;
    settings = {
      service_addr = ":11234";
      domain = "begyn.be";
      imap = {
        server = "imap.begyn.be";
        port = 993;
      };
      smtp = {
        server = "smtp.begyn.be";
        port = 465;
      };
    };
  };

  # mailserver backups
  # borgbase
  services.borgbackup.jobs."borgbase" = {
    paths = [
      "/var/vmail"
      "/var/dkim"
    ];
    repo = "uszi0k1s@uszi0k1s.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat ${config.age.secrets."secrets/data/borgbase/key".path}";
    };
    prune.keep = {
      within = "3d";
      daily = 5;
      weekly = 2;
      monthly = 5;
    };
    environment.BORG_RSH = "ssh -i ${config.age.secrets."secrets/data/borgbase/ssh".path}";
    compression = "auto,lzma";
    startAt = "*:0/20";
  };
}
