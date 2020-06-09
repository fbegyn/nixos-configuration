{ configs, lib, pkgs, ... }:

{
  home-manager.users.francis.programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    aliases = {
      identity = "\"! git config user.name \\\"$(git config user.$1.name)\\\"\; git config user.email \\\"$(git config user.$1.email)\\\"\; git config user.signingkey \\\"$(git config user.$1.signingkey)\\\"\; :\"";
    };
    extraConfig = {
      core.editor = "vim";
      user.useConfigOnly = true;
      user.personal.name = "Francis Begyn";
      user.personal.email = "francis@begyn.be";
      user.robovision.name = "Francis Begyn";
      user.robovision.email = "francis.begyn@robovision.ai";
      user.fom.name = "Francis \\\"TheCy\\\" Begyn";
      user.fom.email = "theci@fom.be";
    };
  };
}
