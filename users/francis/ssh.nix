{ config, pkgs, ... }:

let
  authPath = "/run/user/1000/ssh-agent";
in {
  environment.variables.SSH_AUTH_SOCK = "${authPath}";
  environment.sessionVariables.SSH_AUTH_SOCK = "${authPath}";
  programs = {
    ssh = {
      startAgent = true;
      agentTimeout = "4h";
    };
    gnupg.agent = {
      enableSSHSupport = false;
    };
  };
}
