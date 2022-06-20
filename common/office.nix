{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    libreoffice-fresh-unwrapped
  ];
}
