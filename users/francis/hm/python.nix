{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python310.withPackages
      (ps: with ps; [
        neovim
        yarp
        setuptools
        virtualenv
        jedi
        requests
        opencv
      ]))
      opencv
    ];
}
