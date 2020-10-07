{ pkgs, ... }:

{
  home-manager.users.francis = {
    programs.fzf = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
