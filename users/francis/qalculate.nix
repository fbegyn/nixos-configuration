{ config, pkgs, ... }:

{
  home-manager.users.francis = { home.packages = with pkgs; [ libqalculate ]; };
}
