# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # laptop hardware
    <nixos-hardware/common/pc/laptop>
    <nixos-hardware/common/pc/ssd>
    # specific to thinkpad
    <nixos-hardware/lenovo/thinkpad>
    <nixos-hardware/lenovo/thinkpad/x1>
    <nixos-hardware/common/pc/laptop/acpi_call.nix>
    # wireless settings
    ../../secrets/wireless.nix
    # common settings
    ../../common/cachix.nix
    ../../common/gpg.nix
    ../../common/base.nix
    ../../common/security.nix
    ../../common/pulseaudio.nix
    ../../common/screen-brightness.nix
    ../../common/bluetooth.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/wireguard.nix
    ../../common/master.nix
    ../../common/unstable.nix
    ../../users
    ../../users/francis
    ../../users/francis/gui.nix
    #../../users/francis/configurations/i3
    ../../users/francis/configurations/sway
    ../../services/tailscale.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "horme"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.interfaces.enp0s31f6.useDHCP = true;

  # set trackpoint sensitivity
  hardware.trackpoint.sensitivity = 64;

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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;
  services.hardware.bolt.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

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
}

