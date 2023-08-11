# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./zfs.nix

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
    # ../../common/eid.nix
    ../../common/webcam.nix
    ../../common/video-accel.nix
    ../../common/office.nix
    ../../common/ios.nix
    ../../common/virt.nix

    ../../users
    ../../users/francis
    ../../users/francis/yubikey.nix
    ../../users/francis/gui.nix
    ../../users/francis/inuits.nix
    ../../users/francis/i3

    ../../services/tailscale.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.kernelParams = [ "i915.force_probe=46a8" ];
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.generationsDir.copyKernels = true;

  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.copyKernels = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.zfsSupport = true;
  boot.loader.grub.devices = [
    "/dev/disk/by-id/nvme-eui.e8238fa6bf530001001b448b4b64ab08"
  ];

  boot.supportedFilesystems = [ "ntfs" ];

  nix.settings.auto-optimise-store = true;
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';
  services.nfs.server.enable = true;

  sound.enable = true;

  networking.hostName = "ania"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

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
  networking.networkmanager.enable = true;
  # networking.interfaces.enp2s0f0.useDHCP = true;
  # networking.interfaces.enp5s0.useDHCP = true;
  # networking.interfaces.wlp3s0.useDHCP = true;
  environment.systemPackages = with pkgs; [
    steam-run
  ];

  programs.gnupg.package = pkgs.unstable.gnupg;
  services.tailscale = {
    enable = true;
    package = pkgs.unstable.tailscale;
  };

  home-manager.users.francis.home.stateVersion = "23.05";
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}

