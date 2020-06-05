{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      dunst
    ];

    home.file = {
      ".config/dunst/dunstrc".source = ./dunstrc;
    };
  };
}
