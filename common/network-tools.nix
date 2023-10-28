{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.unstable; [
    ldns
    tcpdump
    nmap
    iperf
    netperf
    socat
  ];

  programs = {
    traceroute.enable = true;
    mtr = {
      enable = true;
      package = pkgs.unstable.mtr;
    };
  };
}
