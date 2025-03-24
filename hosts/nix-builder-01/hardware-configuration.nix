{ config, lib, pkgs, modulesPath, ... }:

{
  fileSystems = {
    "/" = {
      device = "rpool/data/subvol-202-disk-0";
      fsType = "zfs";
    };
    "/nix" = {
      device = "dpool/nix/subvol-202-disk-0";
      fsType = "zfs";
    };
    "/var" = {
      device = "dpool/var/subvol-202-disk-0";
      fsType = "zfs";
    };
  };
}
