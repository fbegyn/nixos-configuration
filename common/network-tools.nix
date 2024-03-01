{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs.unstable; [
    ldns
    tcpdump
    nmap
    iperf
    socat
    mapcidr
  ] ++ (if pkgs.stdenv.isLinux then [pkgs.unstable.net-snmp] else []);
}
