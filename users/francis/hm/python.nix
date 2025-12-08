{ pkgs, ... }:

{
  home.packages = with pkgs.unstable;
    [
      (python313.withPackages
      (ps: with ps; [
        neovim
        yarp
        pip
        setuptools
        virtualenv
        uv
        jedi
        requests
      ]))
    ];
}
