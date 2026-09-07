{ config, lib, pkgs, ... }:

{
  imports = [
    ./fwupd.nix
  ];
  services.logind = {
    settings.Login.HandleLidSwitch = "suspend";
  };
  environment.systemPackages = with pkgs; [
    acpi
  ];
}
