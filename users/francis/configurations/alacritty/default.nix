{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.alacritty
    ];
    xdg.configFile = {
      "alacritty/alacritty.yml".source = ./alacritty.yml;
    };
  };
}
