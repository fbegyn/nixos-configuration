{ pkgs, ... }:

{
  home-manager.users.francis.home.packages = with pkgs; [
    unstable.nomad
    unstable.terraform
    unstable.vagrant
    unstable.packer
  ];
}
