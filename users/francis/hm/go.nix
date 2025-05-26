{ config, pkgs, ... }:

{
  programs.go = {
    enable = true;
    goPath = ".go";
    package = pkgs.unstable.go_1_24;
  };
}
