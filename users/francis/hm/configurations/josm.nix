{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    josm
  ];
}
