# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [
    # ../../common
    ../../common/gpg.nix
    ../../common/network-tools.nix

    ../../users
    ../../users/francis
  ];

  # no EFI partition on containers
  proxmoxLXC.enable = true;
  boot.isContainer = true;
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  systemd.services.zfs-mount.enable = false;

  networking = {
    hostName = "nas-01";
    useDHCP = lib.mkDefault true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22 # ssh
        80 # HTTP
        443 # HTTPS
      ];
    };
  };

  # Enable QEMU Guest for Proxmox
  services.qemuGuest.enable = lib.mkDefault true;

  # Use the boot drive for grub
  boot.loader.grub.enable = lib.mkDefault true;
  boot.loader.grub.devices = [ "nodev" ];

  boot.growPartition = lib.mkDefault true;

  # Default filesystem
  fileSystems."/" = lib.mkDefault {
    device = "/dev/disk/by-label/nixos";
    autoResize = true;
    fsType = "ext4";
  };

  # Allow remote updates with flakes and non-root users
  nix.settings.trusted-users = [ "root" "@wheel" ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Enable mDNS for `hostname.local` addresses
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.publish = {
    enable = true;
    addresses = true;
  };

  # Enable ssh
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };
  programs.ssh.startAgent = true;

  # system packagesystem
  environment.systemPackages = with pkgs; [
    vim
    neovim
    wget
  ];

  virtualisation.podman = {
    enable = true;
    dockerSocket.enable = true;
    defaultNetwork.settings = {
      dns_enabled = true;
    };
  };
  virtualisation.oci-containers = {
    backend = "podman";
  };

  users.users.root.password = "foobar";
  # home-manager.users.francis = {
  #   imports = [
  #     ../../users/francis/hm/go.nix
  #     ../../users/francis/hm/configurations/fish.nix
  #     ../../users/francis/hm/configurations/bash.nix
  #   ];
  # };
  system.stateVersion = lib.mkDefault "25.05"; # Did you read the comment?
}
