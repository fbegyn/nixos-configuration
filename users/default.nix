{ pkgs, home-manager, ... }:

{
  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLQRocoQMC4JDWPL7fYYCfhpq5SoUfaCxUeQpgQrZ7c4Q44/YWPMjHkmGM8+7ZehfVHVxSNJ4i/Rre1n0MJcCT8= macbook-francis@secretive.erebus.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrr2D8eSb+vjbnkkGxmhj8I6hhmiJYFiLE4Xz4hjZKZ francis@eos"
  ];

  programs.fish.enable = true;
  programs.bash = {
    enable = true;
  };

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
    shell = pkgs.bashInteractive;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV"
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLQRocoQMC4JDWPL7fYYCfhpq5SoUfaCxUeQpgQrZ7c4Q44/YWPMjHkmGM8+7ZehfVHVxSNJ4i/Rre1n0MJcCT8= macbook-francis@secretive.erebus.local"
    ];
  };

  home-manager.users = {
    francis = (import ./francis/hm/base.nix);
  };
}
