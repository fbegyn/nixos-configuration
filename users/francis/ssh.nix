{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = false;
    agentTimeout = "4h";
  };
}
