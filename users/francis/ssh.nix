{ config, pkgs, ... }:

{
  programs = {
    ssh = {
      startAgent = false;
      agentTimeout = "4h";
    };
    gnupg.agent = {
      enableSSHSupport = true;
    };
  };

  environment.shellInit = ''
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  '';
}
