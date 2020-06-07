{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      dunst
    ];

    xdg.configFile = {
      "dunst/dunstrc".source = ./dunstrc;
    };
  };
}
