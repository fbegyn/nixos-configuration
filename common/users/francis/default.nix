{ config, pkgs, ... }:

{
  imports = [ ./base.nix ];

  home.packages = with pkgs; [ vim ];
}
