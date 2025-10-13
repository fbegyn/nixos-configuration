{ config, lib, pkgs, modulesPath, ...}:
{
  fileSystems = {
    "/" = {
      device = "rpool/data/subvol-301-disk-0";
      fsType = "zfs";
      autoResize = true;
    };
  };
}
