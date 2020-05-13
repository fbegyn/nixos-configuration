{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./francis/variables.nix
  ];

  programs.zsh.enable = true;

  users.groups.francis.gid = 1000;

  users.users.francis = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "audio" "plugdev" "libvirtd" "adbusers" ];
    group = "francis";
    shell = pkgs.fish;
  };

  home-manager.users.francis = (import ./francis/base.nix);
}
