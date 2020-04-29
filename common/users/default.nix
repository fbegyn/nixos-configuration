{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];



  programs.zsh.enable = true;

  users.groups.francis.gid = 1000;

  users.users.francis = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "audio" "plugdev" "libvirtd" "adbusers" ];
    group = "francis";
    shell = pkgs.zsh;
  };

  home-manager.users.francis = (import ./francis/base.nix);
}
