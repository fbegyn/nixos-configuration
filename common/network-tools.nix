{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.ldns
    unstable.tcpdump
    unstable.nmap
    unstable.iperf
    unstable.netperf
    unstable.socat
  ];

  programs = {
    traceroute.enable = true;
    mtr = {
      enable = true;
      package = pkgs.unstable.mtr;
    };
  };
}
