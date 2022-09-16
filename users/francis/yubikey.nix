{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    paperkey
    yubico-pam
    yubikey-manager
    yubioath-desktop
    yubikey-personalization
  ];

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';

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
    mode = "challenge-response";
  };
}

