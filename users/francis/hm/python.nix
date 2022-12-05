{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python310.withPackages
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
