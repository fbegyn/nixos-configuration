{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = true;
    agentTimeout = "4h";
  };
}
