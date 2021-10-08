{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.ldns
    unstable.telnet
    unstable.tcpdump
  ];

  programs = {
    traceroute.enable = true;
    mtr = {
      enable = true;
      package = pkgs.unstable.mtr;
    };
    wireshark = {
      enable = true;
      package = pkgs.unstable.wireshark;
    };
  };
}
