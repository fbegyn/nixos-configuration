{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.ldns
    unstable.inetutils
    unstable.tcpdump
    unstable.nmap
    unstable.iperf
    unstable.netperf
  ];

  programs = {
    traceroute.enable = true;
    mtr = {
      enable = true;
      package = pkgs.unstable.mtr;
    };
  };
}
