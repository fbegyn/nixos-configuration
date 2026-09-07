# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./zfs.nix
    ./disko.nix

    # laptop hardware
    # <nixos-hardware/common/pc/laptop>
    # <nixos-hardware/common/pc/ssd>
    # <nixos-hardware/common/cpu/intel>

    # common settings
    ../../common/laptop.nix
    ../../common/moonlander.nix
    ../../common/network-tools.nix
    ../../common/networkmanager.nix
    ../../common/gpg.nix
    ../../common/bluetooth.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/wireguard.nix
    ../../common/eid.nix
    ../../common/webcam.nix
    ../../common/video-accel.nix
    ../../common/ios.nix
    ../../common/virt.nix

    ../../users
    ../../users/hm.nix
    ../../users/francis
    ../../users/francis/yubikey.nix
    ../../users/francis/gui.nix
    ../../users/francis/niri

  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "ntfs" "zfs" ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.hostName = "geros"; # Define your hostname.
  networking.useDHCP = false;
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "wpa_supplicant";
  networking.useNetworkd = false; # Less suited for dynamic environments

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
  services.hardware.bolt.enable = true;

  services.zfs = {
    autoSnapshot.enable = true;
    autoScrub.enable = true;
  };

  nix.settings.auto-optimise-store = true;
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  services.openssh = {
    enable = true;
    openFirewall = true;
  };

  # emacs
  home-manager.users.francis.emacs.emacsPackage = pkgs.unstable.emacs31-pgtk;
  services.emacs = {
    enable = true;
    package = config.home-manager.users.francis.emacs.package;
  };

  environment.systemPackages = with pkgs; [
    steam-run
  ];

  programs.gnupg.package = pkgs.unstable.gnupg;
  services.irqbalance.enable = true;

  users.users.root = {
    initialHashedPassword = "$6$CFXMOgQ3c/aIANm1$Rv3hwrZi3HVLUOVgXAM77lmsN8Ef.PaWkNokl39jcrX7VkiqOGVX/dY6sfBpR07CxKu2R5tySok7MfoPczB11/";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}

