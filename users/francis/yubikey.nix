{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    paperkey
    yubico-pam
    yubikey-manager
    yubikey-personalization
    yubioath-flutter
  ];

  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [
    yubikey-manager
    yubikey-personalization
  ];

  security.pam.yubico = {
    enable = false;
    mode = "challenge-response";
  };
}

