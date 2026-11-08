{ config, pkgs, ... }:

{
  services.fprintd.enable = true;
  security.pam.services = {
    swaylock.fprintAuth = false;
    i3lock.fprintAuth = false;
    i3lock-color.fprintAuth = false;
    gdm.fprintAuth = false;
    sddm.fprintAuth = false;
  };
}
