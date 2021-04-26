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
    <nixos-hardware/lenovo/thinkpad/t14>
    # common settings
    ../../common/cachix.nix
    ../../common/gpg.nix
    ../../common/base.nix
    ../../common/security.nix
    ../../common/pulseaudio.nix
    ../../common/bluetooth.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/wireguard.nix
    ../../common/master.nix
    ../../common/unstable.nix
    ../../common/openvpn.nix
    ../../common/eid.nix
    ../../common/liveview-webcam.nix
    ../../common/video-accel.nix
    ../../common/amdgpu.nix

    ../../common/vectera.nix
    ../../common/hashicorp.nix
    ../../common/libvirt.nix

    ../../users
    ../../users/francis
    ../../users/francis/gui.nix
    ../../users/francis/configurations/i3
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.cleanTmpDir = true;

  nix.autoOptimiseStore = true;
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  services.xserver = {
    enable = true;
  };

  networking.hostName = "ania"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager = {
    enable = true;
    insertNameservers = [ "10.5.1.10" ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;
  services.hardware.bolt.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.interfaces.enp2s0f0.useDHCP = true;
  # networking.interfaces.enp5s0.useDHCP = true;
  # networking.interfaces.wlp3s0.useDHCP = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

