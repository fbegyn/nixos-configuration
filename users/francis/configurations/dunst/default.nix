{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.dunst
    ];

    xdg.configFile = {
      "dunst/dunstrc".source = ./dunstrc;
    };
  };
}
