{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    paperkey
    yubico-pam
    yubikey-manager
    yubioath-desktop
  ];

  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  security.pam.yubico = {
    enable = false;
    debug = true;
    mode = "challenge-response";
  };
}
