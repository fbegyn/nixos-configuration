{ config, pkgs, ... }:

{
  imports = [<home-manager/nixos>];

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
      "vboxusers"
      "adbusers"
      "input"
      "lp"
      "scanner"
      "networkmanager"
    ];
    group = "francis";
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKjrK5P5gwQYt4GOxGtGoHk6Yj8vMVk7Sp9X6EXOWo4i francis@horme"
    ];
  };

  home-manager.users = {
    francis = (import ./francis/core.nix);
  };
}
