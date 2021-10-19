{ config, pkgs, ... }:

{
  services.fprintd.enable = true;
  security.pam.services = {
    login.fprintAuth = true;
    swaylock.fprintAuth = true;
    i3lock.fprintAuth = true;
    i3lock-color.fprintAuth = true;
    gdm.fprintAuth = true;
  };
}
