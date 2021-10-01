{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.ldns
    unstable.mtr
    unstable.telnet
    unstable.tcpdump
    unstable.wireshark
  ];
}
