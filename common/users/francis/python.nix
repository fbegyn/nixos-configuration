{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (python38.withPackages (ps: with ps; [ Mako requests pyyaml neovim yarp ]))
  ];
}
