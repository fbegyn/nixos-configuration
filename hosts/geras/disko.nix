{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-eui.000000000000000100a0752554b6cc21"; # Change this!
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };

            swap = {
              size = "17G";
              type = "8200";
              content = {
                type = "swap";
                randomEncryption = true;
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
          };
        };
      };
    };

    zpool = {
      zroot = {
        type = "zpool";
        mode = "";
        rootFsOptions = {
          compression = "zstd";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
          relatime = "on";
          encryption = "aes-256-gcm";
          keyformat = "passphrase";
          keylocation = "prompt";
          canmount = "off";
          mountpoint = "none";
        };
        options = {
          ashift = "12";
          autotrim = "on";
        };

        datasets = {
          "root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options = {
              canmount = "on";
              quota = "100G";
            };
          };

          # Nix store + supporting paths, capped at 150G
          "nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options = {
              canmount = "on";
              refquota = "150G";
            };
          };

          # Separate dataset for nix daemon state (logs, DB, profiles)
          "nix/var" = {
            type = "zfs_fs";
            mountpoint = "/nix/var";
            options = {
              canmount = "on";
              quota = "8G";
            };
          };

          # Home, capped at 250G
          "home" = {
            type = "zfs_fs";
            mountpoint = "/home";
            options = {
              canmount = "on";
              quota = "250G";
            };
          };

          "games" = {
            type = "zfs_fs";
            mountpoint = "/home/francis/Games";
            options = {
              recordsize = "1M";
              refquota = "50G";
            };
          };
          "games/steam" = {
            type = "zfs_fs";
            mountpoint = "/home/francis/.local/share/Steam";
            options = {
              recordsize = "1M";
              quota = "10G";
            };
          };

          "var" = {
            type = "zfs_fs";
            mountpoint = "/var";
            options = {
              canmount = "on";
              quota = "64G";
            };
          };

          "var-log" = {
            type = "zfs_fs";
            mountpoint = "/var/log";
            options = {
              canmount = "on";
              quota = "32G";
            };
          };

          "var-lib" = {
            type = "zfs_fs";
            mountpoint = "/var/lib";
            options = {
              canmount = "on";
              quota = "64G";
            };
          };

          "var-lib-containers" = {
            type = "zfs_fs";
            mountpoint = "/var/lib/containers";
            options = {
              canmount = "on";
              "com.sun:auto-snapshot" = "false";
              quota = "128G";
            };
          };

          "tmp" = {
            type = "zfs_fs";
            mountpoint = "/tmp";
            options = {
              canmount = "on";
              sync = "disabled";
            };
          };
        };
      };
    };
  };

  # Ensure home mountpoint parent exists before ZFS mounts
  systemd.tmpfiles.rules = [
    "d /home/francis 0755 francis users -"
  ];
}
