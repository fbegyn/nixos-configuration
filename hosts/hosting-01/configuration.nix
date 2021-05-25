# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../../common
    ../../common/security.nix

    ../../users
    ../../users/francis

    ../../services/website
    ../../services/tailscale.nix

    ./acme.nix
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "hosting-01"; # Define your hostname.
  time.timeZone = "Europe/Brussels"; # Set your time zone.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.ens10.useDHCP = true;
  networking.interfaces.ens3.useDHCP = true;
  networking.defaultGateway6 = {
    address = "fe80::1";
    interface = "ens3";
  };
  networking.interfaces.ens3.ipv6 = {
    addresses = [
      {
        address = "2a01:4f9:c010:d553::1";
        prefixLength = 64;
      }
    ];
  };
  networking.firewall.interfaces = {
    "tailscale0" = {
      allowedTCPPorts = [ 22 56697 8000 9000 ];
    };
  };
  networking.firewall = {
    allowedTCPPorts = [ 80 443 ];
  };

  thecy.services.website = {
    enable = true;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  # IRC bouncer
  services.znc = {
    enable = true;
    openFirewall = false;
    useLegacyConfig = false;
    config = {
      LoadModule = [ "adminlog" "webadmin" ];
      Listener.listener0.Port = 56697;
      User.fbegyn = {
        Admin = true;
        Nick = "fbegyn";
        AltNick = "thecy";
        LoadModule = [ "chansaver" "controlpanel" ];
      };
    };
  };

  # weechat
  environment.systemPackages = [ pkgs.unstable.weechat ];
  home-manager.users.francis.systemd.user.services.weechat = {
    Unit = {
      Description = "weechat headless";
    };
    Service = {
      ExecStart = "${pkgs.unstable.weechat}/bin/weechat-headless --stdout";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  services.nginx.virtualHosts = {
    "irc.francis.begyn.be" = {
      forceSSL = true;
      useACMEHost = "francis.begyn.be";
      locations."^~ /weechat" = {
        proxyPass = "http://127.0.0.1:9000";
        proxyWebsockets = true;
      };
    };   
  };
  # services.oauth2.proxy = {
  #   enable = true;
  #   email.addresses = ''
  #     francis@begyn.be
  #   '';
  #   nginx.virtualhosts = [
  #     "irc.francis.begyn.be"
  #   ];
  #   clientID = "";
  #   keyFile = "";
  # };

  # tailscale machine specific
  thecy.services.tailscale = let
    hosts = import ../../secrets/hosts.nix;
  in {
    enable = true;
    autoprovision = {
      enable = true;
      key = "${hosts.hosting-01.tailscale.oneoffkey}";
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

