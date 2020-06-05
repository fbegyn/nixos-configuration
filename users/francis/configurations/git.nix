{ configs, lib, pkgs, ... }:

{
  home-manager.users.francis.programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    ignores = [];
    aliases = {
      identity = "\"! git config --add user.name \\\"$(git config user.$1.name)\\\"; git config user.email \\\"$(git config user.$1.email)\\\";:\"";
    };
    extraConfig = {
      core.editor = "vim";
      user.useConfigOnly = true;
      user.personal.name = "\"Francis Begyn\"";
      user.personal.email = "francis@begyn.be";
      user.robovision.name = "\"Francis Begyn\"";
      user.robovision.email = "francis.begyn@robovision.ai";
      user.fom.name = "\"Francis \\\"TheCy\\\" Begyn\"";
      user.fom.email = "theci@fom.be";
    };
  };
}
