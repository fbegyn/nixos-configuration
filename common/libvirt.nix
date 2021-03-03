{ config, pkgs, ...}:

{
  virtualisation.libvirtd.enable = true;
  boot.extraModprobeConfig = "options kvm_intel nested=1";
}
