# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      ../../common
      ../../common/security.nix
      ../../common/bluetooth.nix

      ../../users

      # load in set of services to run
      ../../services/grafana
      ../../services/prometheus
      ../../services/coredns
      ../../services/unifi
      ../../services/traefik
      ../../services/consul
      ../../services/node-exporter
      ../../services/blackbox-exporter
      ../../services/snmp-exporter
      ../../services/speedtest-exporter
      ../../services/loki
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ares"; # Define your hostname.
  networking.wireless = {
    enable = false;
    interfaces = [ "wlp3s0" ];
  };

  # nixops 2 - non-root running
  nix.trustedUsers = ["francis"];
  users.users.francis.extraGroups = ["wheel"];
  security.sudo.wheelNeedsPassword = false;

  # disable the laptop lid switch
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces = {
    eno1.ipv4.addresses = [
      {
        address = "10.3.10.10";
        prefixLength = 16;
      }
      {
        address = "10.3.2.2";
        prefixLength = 16;
      }
    ];
    wlp3s0.useDHCP = true;
  };

  networking = {
    defaultGateway = "10.3.1.1";
    nameservers = ["10.3.2.2" "10.3.1.1" "1.1.1.1"];
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    permitRootLogin = "prohibit-password";
    passwordAuthentication = true;
    extraConfig = ''
      Compression no
      AuthorizedKeysFile .ssh/authorized_keys
    '';
  };

  # no local firewall required on the server
  networking.firewall.enable = false;

  # AMD drivers

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  #services.xserver.displayManager.lightdm.enable = true;
  #services.xserver.displayManager.defaultSession = "none+i3";
  #services.xserver.windowManager.i3.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

