{ config, lib, pkgs, ... }:

{
  imports = [
    ./fwupd.nix
  ];
  services.logind = {
    lidSwitch = "suspend";
  };
  environment.systemPackages = with pkgs; [
    acpi
  ];
}
