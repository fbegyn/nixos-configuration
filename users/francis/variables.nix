{ config, pkgs, ... }:

{
  environment.variables = {
    PAGER = "less";
  };
}

