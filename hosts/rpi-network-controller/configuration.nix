{ config, pkgs, lib, ... }:
{
  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;

  # kernel packages for RPI 2 and newer
  boot.kernelPackages = pkgs.linuxPackages_5_4;

  boot.kernelParams = [
    "cma=256M"
  ];
  boot.loader.raspberryPi = {
    enable = true;
    version = 3;
    uboot.enable = true;
    firmwareConfig = ''
      gpu_mem=256
    '';
  };

  environment.systemPackages = with pkgs; [
    raspberrypi-tools
    vim
  ];

  documentation.nixos.enable = false;
  nix.gc = {
    automatic = true;
    options = "--delete-older-then 15d";
  };

  boot.consoleLogLevel = 7;

  # File systems configuration for using the installer's partition layout
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

  networking = {
    hostName = "network-controller";
    interfaces.eth0 = {
      useDHCP = true;
    };
  };

  # enable SSH on startup
  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
  };

  # setup francis user
  users.groups.francis.gid = 1000;
  users.users.francis = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    group = "francis";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL+xR6Eik2A8Ofzdbl6uosvr9k0w6gO54n3AQa+bpaz5 francis@francis-xps13"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKjrK5P5gwQYt4GOxGtGoHk6Yj8vMVk7Sp9X6EXOWo4i francis@horme"
    ];
  };

  security.sudo.wheelNeedsPassword = false;
}
