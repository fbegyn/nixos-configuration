{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = true;
    agentTimeout = "2h";
  };
}
