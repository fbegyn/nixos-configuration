{ config, lib, pkgs, modulesPath, ... }:

{
  fileSystems = {
    "/" = {
      device = "rpool/data/subvol-201-disk-0";
      fsType = "zfs";
    };
  };
}
