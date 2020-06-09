{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      alacritty
    ];
    xdg.configFile = {
      "alacritty/alacritty.yml".source = ./alacritty.yml;
    };
  };
}
