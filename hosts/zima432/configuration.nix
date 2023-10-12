# Edit this configuration file to define what should be installed on
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../common
    ../../common/wireguard.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv4.ip_forward" = true;
    "net.ipv6.conf.all.forwarding" = true;
    "net.ipv6.ip_forward" = true;
    "net.ipv6.conf.ppp0.accept_ra" = 2;
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  nix = {
    distributedBuilds = true;
    buildMachines = [ {
      hostName = "10.5.1.10";
      system = "x86_64-linux";
      maxJobs = 2;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      sshUser = "francis";
      sshKey = "/home/francis/.ssh/id_rsa";
    } ];
  };

  # Interfaces networking setup
  networking = {
    hostName = "zima432"; # Define your hostname.
    nameservers = [ "1.1.1.1" "10.5.1.10" "8.8.8.8" ];
    useDHCP = false;
    interfaces = {
      enp2s0 = {
        useDHCP = true;
      };
      enp3s0 = {
        useDHCP = true;
      };
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.francis = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    git
    git-crypt
    vim
    htop
    screen
    inetutils
    ipset
  ];

  services.prometheus.exporters = {
    node = {
      enable = true;
    };
  };

  home-manager.users.francis.home.stateVersion = "23.05";
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

