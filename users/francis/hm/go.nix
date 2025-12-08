{ config, pkgs, ... }:

{
  programs.go = {
    enable = true;
    env.GOPATH = "~/.go";
    package = pkgs.unstable.go_1_25;
  };
}
