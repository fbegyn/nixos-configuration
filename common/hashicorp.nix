{ pkgs, ... }:

{
  home-manager.users.francis.home.packages = with pkgs; [
    master.nomad
    master.terraform
    master.vagrant
    master.packer
  ];
}
