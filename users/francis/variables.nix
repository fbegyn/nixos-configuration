{ config, pkgs, ... }:

{
  environment.variables = {
    WINIT_X11_SCALE_FACTOR = "1.0";
    PAGER = "less";
    EDITOR = "nvim";
    LESS = "-eFRX";
  };
}

