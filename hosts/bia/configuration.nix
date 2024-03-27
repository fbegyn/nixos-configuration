# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    # <nixos-hardware/common/cpu/amd>
    ./hardware-configuration.nix
    ../../common/printer.nix
    ../../common/scanner.nix
    ../../roles/workstation
    ../../roles/gaming

    ../../users
    ../../users/francis
    ../../users/francis/gui.nix
    ../../users/francis/i3
    ../../services/tailscale.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" "wasm32-wasi" ];

  # nvidia
  boot.kernelParams = [ "nvidia-drm.modeset=1" ];
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

  programs.adb.enable = true;

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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

