{ config, pkgs, ... }:

{
  environment.variables = {
    PAGER = "less";
    BROWSER = "chromium-browser";
    WINIT_X11_SCALE_FACTOR = "1.0";
    NIX_SSHOPTS = "-t";
  };
}

