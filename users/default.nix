{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./francis
  ];

  # user francis
  users.groups.francis.gid = 1000;
  users.users.francis = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
      "audio"
      "plugdev"
      "libvirtd"
      "adbusers"
      "input"
      "lp"
      "scanner"
    ];
    group = "francis";
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL+xR6Eik2A8Ofzdbl6uosvr9k0w6gO54n3AQa+bpaz5 francis@francis-xps13"
    ];
  };
}
