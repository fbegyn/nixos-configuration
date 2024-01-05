{ config, pkgs, ...}:

{
  virtualisation.libvirtd.enable = true;
  # virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = [ "francis" ];
  boot.extraModprobeConfig = "options kvm_intel nested=1";
  environment.systemPackages = with pkgs.unstable; [
    virt-manager
  ];

  virtualisation.docker = {
    storageDriver = "overlay2";
  };
}
