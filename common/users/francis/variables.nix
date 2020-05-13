{ config, pkgs, ... }:

{
  environment.variables = {
    EDITOR = "nvim";
    PAGER = "less";
  };
}

