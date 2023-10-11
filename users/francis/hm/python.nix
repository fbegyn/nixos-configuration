{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python311.withPackages
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
