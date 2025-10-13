{ config, lib, pkgs, modulesPath, ...}:
{
  fileSystems = {
    "/" = {
      device = "dpool/storage/data/subvol-301-disk-0";
      fsType = "zfs";
    };
    "/var/log" = {
      device = "dpool/dvar/log/subvol-301-disk-0";
      fsType = "zfs";
    };
    "/var" = {
      device = "dpool/dvar/subvol-301-disk-0";
      fsType = "zfs";
    };
    "/home" = {
      device = "dpool/home/subvol-301-disk-0";
      fsType = "zfs";
    };
  };
}
