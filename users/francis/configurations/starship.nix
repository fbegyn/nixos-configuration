{ config, pkgs, ... }:

{
  home-manager.users.francis = {
    programs.starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };
  };
}
