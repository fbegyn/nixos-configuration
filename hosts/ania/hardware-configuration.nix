# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ddb62988-8b4c-47de-8963-f16247896e40";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."system".device = "/dev/disk/by-uuid/40d9ec79-d296-4772-8eb2-3cb57e3c1cb5";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/191C-FBFD";
      fsType = "vfat";
    };

  swapDevices = [ ];

}
