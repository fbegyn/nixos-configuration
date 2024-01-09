# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    # <nixos-hardware/common/cpu/amd>
    ./hardware-configuration.nix
    ../../common
    ../../common/bluetooth.nix
    ../../common/moonlander.nix
    ../../common/ios.nix
    ../../common/gpg.nix
    ../../common/office.nix
    ../../common/steam.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/scanner.nix
    ../../common/eid.nix
    ../../common/rtlsdr.nix
    ../../common/webcam.nix
    ../../common/network-tools.nix
    ../../common/video-accel.nix

    ../../users
    ../../users/francis
    ../../users/francis/gui.nix
    ../../users/francis/i3
    ../../services/tailscale.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" "wasm32-wasi" ];

  sound.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = false;
    open = false;
    nvidiaSettings = true;
  };

  services.prometheus.exporters.node.enable = true;
  services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];

  networking.hostName = "bia";
  networking.hostId = "6c19e3cb";
  # After the Greek personification of force and raw energy
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.vscode-server = {
    enable = true;
    enableFHS = true;
    nodejsPackage = pkgs.unstable.nodejs_18;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
  ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  programs.steam.remotePlay.openFirewall = true;

  fbegyn.x.xautolock = false;

  # tailscale machine specific
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  # configure the systems wake on lan settings
  systemd.services.wol-setup = {
    enable = true;
    description = "Configure WoL on boot";
    after = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      RemainAfterExit = "true";
      ExecStart = "${pkgs.ethtool}/bin/ethtool -s enp4s0 wol g";
    };
    wantedBy = [ "multi-user.target" ];
  };

  home-manager.users.francis.home.stateVersion = "23.05";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

