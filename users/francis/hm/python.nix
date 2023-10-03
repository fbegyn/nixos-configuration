{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python312.withPackages
      (ps: with ps; [
        neovim
        yarp
        pip
        setuptools
        virtualenv
        jedi
        requests
      ]))
    ];
}
