{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python39.withPackages
      (ps: with ps; [
        neovim
        yarp
        setuptools
        virtualenv
        jedi
        requests
      ]))
    ];
}
