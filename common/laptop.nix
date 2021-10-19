{ config, lib, pkgs, ... }:

{
  services.logind = {
    lidSwitch = "suspend";
    lidSwitchExternalPower = "lock";
  };
}
