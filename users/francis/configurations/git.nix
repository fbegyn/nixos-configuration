{ pkgs, ... }:

{
  home.packages = [ pkgs.git-crypt ];
  programs.git = {
    package = pkgs.unstable.gitAndTools.gitFull;
    enable = true;
    aliases = {
      identity = "! git config user.name \"$(git config user.$1.name)\"; git config user.email \"$(git config user.$1.email)\"; git config user.signingkey \"$(git config user.$1.signingkey)\"; :";
    };
    extraConfig = {
      core.editor = "vim";
      user.useConfigOnly = true;
      user.personal.name = "Francis Begyn";
      user.personal.email = "francis@begyn.be";
      user.personal.signingkey = "";
      user.fom.name = "Francis Begyn - Thecy";
      user.fom.email = "theci@fom.be";
      user.fom.signingkey = "";
      init.defaultBranch = "main";
      pull.rebase = false;
    };
  };
  
}
