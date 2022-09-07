{ config, pkgs, ... }:

{
  environment.variables = {
    PAGER = "less";
    BROWSER = "chromium-browser";
  };
}

