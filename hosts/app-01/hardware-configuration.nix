{ config, lib, pkgs, modulesPath, ... }:

{
  fileSystems = {
    "/" = {
      device = "rpool/data/subvol-103-disk-0";
      fsType = "zfs";
    };
    "/nix" = {
      device = "dpool/nix/subvol-103-disk-0";
      fsType = "zfs";
    };
    "/home" = {
      device = "dpool/home/subvol-103-disk-0";
      fsType = "zfs";
    };
    "/var" = {
      device = "dpool/var/subvol-103-disk-0";
      fsType = "zfs";
    };
    "/var/log" = {
      device = "dpool/var/log/subvol-103-disk-0";
      fsType = "zfs";
    };
  };
}

