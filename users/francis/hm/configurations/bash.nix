{ config, pkgs, ...}:

{
  home.file.".inputrc".text = ''
    # Key bindings, up/down arrow searches through history
    "\e[A": history-search-backward
    "\e[B": history-search-forward
    "\eOA": history-search-backward
    "\eOB": history-search-forward
  '';
  programs.autojump.enable = true;
  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      "vim" = "nvim";
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
      "ls" = "ls --color";
      "gitr" = "git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5";
      "dgitr" = "bash -c 'for d in ./*/; do (echo $d && cd $d && git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5); done'";
    };
    sessionVariables = {
      WINIT_X11_SCALE_FACTOR = "1.0";
      BROWSER = "firefox";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 0;
      QT_SCALE_FACTOR = 1;
      GDK_SCALE = 1;
      GDK_DPI_SCALE = 1;
      MOZ_ENABLE_WAYLAND = 1;
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
    initExtra = let
      gitPrompt = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh";
        hash = "sha256-f/cY9KBv0KC+ft/vkmq7QbE1PEjBUVrTEtImllt0lDo=";
      };
      gitCompletion = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash";
        hash = "sha256-ipZFtgByavKtW4W/S1yecVpL8c3mE4VIMA1zs8OIe48=";
      };
    in ''
      TEMPPID=$(echo $PPID)
      TEMPPROC=$(${pkgs.procps}/bin/ps -o 'comm' -p $TEMPPID | tail -n +2)
      if [[ "''${TEMPPROC##*/}" != "fish" && -z ''${BASH_EXECUTION_STRING} ]] then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
      else
          # load git-prompt script
          . ${gitPrompt}
          # load git completions
          . ${gitCompletion}

          PS1='\[\e[32m\]\u\[\e[0m\]@\[\e[38;5;126m\]\h\[\e[0m\] \[\e[38;5;40m\]\w\[\e[38;5;147m\]$(__git_ps1 " (%s)")\[\e[0m\]> '
      fi
    '';
  };
}
