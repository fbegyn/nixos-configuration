{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = true;
    agentTimeout = "2h";
  };
  home-manager.users.francis = {
    home.packages = with pkgs; [
      sshpass
    ];
  };
}
