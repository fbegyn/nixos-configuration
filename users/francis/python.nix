{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      (python38.withPackages
      (ps: with ps; [
        neovim
        yarp
        setuptools
        virtualenv
        jedi
        python-language-server
      ]))
    ];
}
