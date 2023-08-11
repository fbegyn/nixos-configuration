{ config, pkgs, ... }:

{
  programs = {
    ssh = {
      startAgent = true;
      agentTimeout = "4h";
    };
    gnupg.agent = {
      enableSSHSupport = false;
    };
  };

  # environment.shellInit = ''
  #   gpg-connect-agent /bye
  #   export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  # '';
}
