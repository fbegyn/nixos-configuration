{ configs, lib, pkgs, ... }:

{
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Francis Begyn";
    userEmail = "francis@begyn.be";
    ignores = [];
    extraConfig = {
      core.editor = "vim";
    };
  };
}
