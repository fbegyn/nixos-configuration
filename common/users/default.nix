{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
  ];

  users.users.francis = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "audio" "plugdev" "libvirtd" "adbusers" ];
  };

  home-manager.users.francis = (import ./francis/base.nix);
}
