{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    paperkey
    yubico-pam
    yubikey-manager
    yubioath-desktop
  ];

  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
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

