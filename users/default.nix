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
    extraGroups = [ "wheel" "docker" "audio" "plugdev" "libvirtd" "adbusers" ];
    group = "francis";
    shell = pkgs.fish;
  };
}
