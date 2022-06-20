{ config, lib, pkgs, ... }:

{
  imports = [
    ./fwupd.nix
  ];
  services.logind = {
    lidSwitch = "suspend";
  };
}
