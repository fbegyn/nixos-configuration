# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixos-hardware, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # wireless settings
    ../../secrets/wireless.nix
    # common settings
    ../../common/moonlander.nix
    ../../common/networkmanager.nix
    ../../common/ios.nix
    ../../common/laptop.nix
    ../../common/gpg.nix
    ../../common/office.nix
    ../../common/bluetooth.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/wireguard.nix
    ../../common/webcam.nix
    ../../common/eid.nix
    ../../common/rtlsdr.nix
    ../../common/network-tools.nix
    ../../users
    ../../users/francis
    ../../users/francis/gui.nix
    ../../users/francis/yubikey.nix
    ../../users/francis/sway
    ../../services/tailscale.nix
  ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "horme"; # Define your hostname.
  networking.hostId = "009c169a";
  networking.networkmanager.enable = true;
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.wireless.interfaces = [ "wlp0s20f3" ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.interfaces.enp0s31f6.useDHCP = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "nl_BE.UTF-8";
    supportedLocales = [
      "nl_BE.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LC_MESSAGES = "en_US.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # enable tailscale
  fbegyn.services.tailscale.enable = true;

  security.pki.certificateFiles = [
    "/home/francis/Documents/certs/ca.crt"
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;
  services.hardware.bolt.enable = true;

  # trackpoint settings
  hardware.trackpoint = {
    enable = true;
    sensitivity = 64;
    speed = 90;
    emulateWheel = true;
  };

  # system backups
  services.restic.backups = {
    gdrive = {
      user = "francis";
      paths = [ "/home/francis/Documents" ];
      passwordFile = "/etc/nixos/secrets/key01";
      repository = "rclone:personal-gdrive:/Documents/Backups/Restic";
      timerConfig = {
        onCalendar = "saturday 23:15";
        RandomizedDelaySec = "2h";
      };
    };
  };

  virtualisation.podman.enable = true;
  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs.unstable; [
    sshpass
    steam-run
    qmapshack
    garmindev
    docker-compose
    podman-compose
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}

