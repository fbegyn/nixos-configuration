{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.traceroute
    unstable.ldns
    unstable.mtr
    unstable.telnet
    unstable.tcpdump
    unstable.wireshark
    unstable.whois
  ];
}
