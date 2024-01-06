{ pkgs, home-manager, ... }:

{
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHGwhmmWLgfOpsv1U4bhVqc95jBXK9KpuDjJxSYGiMzE francis@horme"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFwnEB+L6Kk7QNXsQblBbavrd2bdmIRxGPKzZY7n+FnB francis@bia"
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLQRocoQMC4JDWPL7fYYCfhpq5SoUfaCxUeQpgQrZ7c4Q44/YWPMjHkmGM8+7ZehfVHVxSNJ4i/Rre1n0MJcCT8= macbook-francis@secretive.erebus.local"
  ];

  # user francis
  users.groups.francis.gid = 1000;
  users.users.francis = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
      "audio"
      "video"
      "plugdev"
      "libvirtd"
      "libvirt"
      "vboxusers"
      "adbusers"
      "input"
      "fuse"
      "lp"
      "scanner"
      "networkmanager"
      "wireshark"
      "usbmuxd"
    ];
    group = "francis";
    shell = pkgs.unstable.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHGwhmmWLgfOpsv1U4bhVqc95jBXK9KpuDjJxSYGiMzE francis@horme"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFwnEB+L6Kk7QNXsQblBbavrd2bdmIRxGPKzZY7n+FnB francis@bia"
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLQRocoQMC4JDWPL7fYYCfhpq5SoUfaCxUeQpgQrZ7c4Q44/YWPMjHkmGM8+7ZehfVHVxSNJ4i/Rre1n0MJcCT8= macbook-francis@secretive.erebus.local"
    ];
  };

  home-manager.users = {
    francis = (import ./francis/hm/base.nix);
  };
}
