{ pkgs, ... }:

{
  home.packages = with pkgs; [ git-crypt gh ];
  programs.git = {
    package = pkgs.unstable.gitAndTools.gitFull;
    enable = true;
    aliases = {
      identity = "! git config user.name \"$(git config user.$1.name)\"; git config user.email \"$(git config user.$1.email)\"; git config user.signingkey \"$(git config user.$1.signingkey)\"; :";
      wip = "for-each-ref --sort='authordate:iso8601' --format=' %(color:green)%(authordate:relative)%09%(color:white)%(refname:short)' refs/heads";
      sync = "git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5";
    };
    extraConfig = {
      core.editor = "vim";
      init.defaultBranch = "main";
      pull.rebase = true;
      push.autoSetupRemote = true;
      rebase.autosquash = true;
      commit.verbose = true;
      diff.algorithm = "histogram";
      transfer.fsckobjects = true;
      fetch.fsckobjects = true;
      receive.fsckObjects = true;
      url."ssh://git@github.com:".insteadOf = "git://github.com";
      rerere.enabled = true;
      # identities
      user.useConfigOnly = true;
      user.personal.name = "Francis Begyn";
      user.personal.email = "francis@begyn.be";
      user.personal.signingkey = "";
      user.fom.name = "Francis Begyn - Thecy";
      user.fom.email = "theci@fom.be";
      user.fom.signingkey = "";
      user.inuits.name = "Francis Begyn";
      user.inuits.email = "fbegyn@inuits.eu";
      user.inuits.signingkey = "";
    };
  };
}
