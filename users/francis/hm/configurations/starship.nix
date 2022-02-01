{ pkgs, ... }:

{
  programs.starship = {
    enable = true;
    package = pkgs.unstable.starship;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
