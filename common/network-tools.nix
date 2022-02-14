{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.ldns
    unstable.telnet
    unstable.tcpdump
    unstable.ncat
    unstable.iperf
  ];

  programs = {
    traceroute.enable = true;
    mtr = {
      enable = true;
      package = pkgs.unstable.mtr;
    };
  };
}
