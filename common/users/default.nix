{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  programs.zsh.enable = true;

  users.users.francis = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "audio" "plugdev" "libvirtd" "adbusers" ];
    shell = pkgs.zsh;
  };

  home-manager.users.francis = (import ./francis/base.nix);
}
