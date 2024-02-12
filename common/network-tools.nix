{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.unstable; [
    ldns
    tcpdump
    nmap
    iperf
    socat
    mapcidr
  ];
}
