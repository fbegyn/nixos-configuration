{ config, lib, pkgs, modulesPath, ... }:

{
  fileSystems = {
    "/nix/store" = {
      device = "dpool/nix/subvol-202-disk-0";
      fsType = "zfs";
    };
    "/var/log" = {
      device = "dpool/var/log/subvol-202-disk-0";
      fsType = "zfs";
    };
    "/var" = {
      device = "dpool/var/subvol-202-disk-0";
      fsType = "zfs";
    };
    "/" = {
      device = "rpool/data/subvol-202-disk-0";
      fsType = "zfs";
    };
  };
}
