{ config, pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
  };

  environment.systemPackages = [
    pkgs.pinentry
  ];
}

