{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    pgcli
  ];
}
