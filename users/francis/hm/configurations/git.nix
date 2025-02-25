{ pkgs, ... }:

# sources:
# - https://blog.gitbutler.com/how-git-core-devs-configure-git/
{
  home.packages = with pkgs; [ git-crypt gh ];
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    aliases = {
      identity = "! git config user.name \"$(git config user.$1.name)\"; git config user.email \"$(git config user.$1.email)\"; git config user.signingkey \"$(git config user.$1.signingkey)\"; :";
      wip = "for-each-ref --sort='authordate:iso8601' --format=' %(color:green)%(authordate:relative)%09%(color:white)%(refname:short)' refs/heads";
      sync = "git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5";
    };
    extraConfig = {
      # clearly better
      branch.sort = "-committerdate";
      column.ui = "auto";
      tag.sort = "version:refname";
      init.defaultBranch = "main";
      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        mnemonicPrefix = true;
        renames = true;
      };
      push = {
        default = "simple";
        autoSetupRemote = true;
        followTags = true;
      };
      fetch = {
        prune = true;
        pruneTage = true;
        all = true;
        fsckobjects = true;
      };
      # why not?
      help.autocorrect = true;
      commit.verbose = true;
      rerere = {
        enabled = true;
        autoupdate = true;
      };
      root.excludesfile = "~/.gitignore";
      rebase = {
        autoSquash = true;
        autoStash = true;
        updaterefs = true;
        instructionFormat = "<%an/%ad> %s - b:%d";
      };
      # basic
      core.editor = "nvim";
      merge.conflictstyle = "zdiff3";
      pull.rebase = true;
      receive.fsckObjects = true;
      transfer.fsckobjects = true;
      url."ssh://git@github.com:".insteadOf = "git://github.com";
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
      user.vdab.name = "Francis Begyn";
      user.vdab.email = "francis.begyn@vdab.be";
      user.vdab.signingkey = "";
    };
  };
}
