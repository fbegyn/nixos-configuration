{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      maim
      picom
      i3
      feh
    ];

    home = {
      file.".config/i3/config".source = ./config;
    };
  };
}
