{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.unstable; [
    gnupg
    paperkey
    yubico-pam
    yubikey-manager
    yubikey-personalization
  ];

  services.pcscd.enable = true;
  services.udev.packages = with pkgs.unstable; [
    yubikey-personalization
  ];

  security.pam.yubico = {
    enable = false;
    mode = "challenge-response";
  };
}

