{ pkgs, ... }:

{
  home-manager.users.francis.home.packages = with pkgs;
    [
      (python38.withPackages
        (ps: with ps; [ neovim yarp setuptools virtualenv]))
    ];
}
