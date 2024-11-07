{
  disko.devices = {
    disk = {
      type = "disk";
      device = "/dev/sda";
      partitions = {
        boot = { size = "16M"; type = "EF20"; priority = 1; };
        ESP = {
          size = "512M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
          };
        };
        root = {
          size = "40G";
          content = {
            type = "zfs";
            pool = "zroot";
          };
        };
        data = {
          size = "100%";
          content = {
            type = "zfs";
            pool = "zdata";
          };
        };
      };
    };
  };
  zpool = {
    zroot = {
      type = "zpool";
      mode = "";
      datasets = {
        root = {
          type = "zfs_fs";
          mountpoint = "/";
          options.mountpoint = "legacy";
        };
        nix = {
          type = "zfs_fs";
          mountpoint = "/nix/store";
          options.mountpoint = "legacy";
        };
      };
    };
    zdata = {
      type = "zpool";
      mode = "";
      datasets = {
        home = {
          type = "zfs_fs";
          mountpoint = "/home";
        };
        var = {
          type = "zfs_fs";
          mountpoint = "/var";
        };
        logs = {
          type = "zfs_fs";
          mountpoint = "/var/log";
        };
      };
    };
  };
}
