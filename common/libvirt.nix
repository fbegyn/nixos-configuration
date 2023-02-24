{ config, pkgs, ...}:

{
  virtualisation.libvirtd.enable = true;
  boot.extraModprobeConfig = "options kvm_intel nested=1";
  environment.systemPackages = with pkgs.unstable; [
    virt-manager
  ];
}
