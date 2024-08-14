{ pkgs, ...}:

{
  programs.autojump.enable = true;
  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
      "ls" = "ls --color";
    };
    sessionVariables = {
      WINIT_X11_SCALE_FACTOR = "1.0";
      BROWSER = "chromium-browser";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 0;
      QT_SCALE_FACTOR = 1;
      GDK_SCALE = 1;
      GDK_DPI_SCALE = 1;
      MOZ_ENABLE_WAYLAND = 1;
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
    initExtra = let
      gitPrompt = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh";
        sha256 = "1bkdllwxfbcbflfi6w4p2ls8hvqpv2hwvqf5fw3w4zh89p2vg5ra";
      };
      gitCompletion = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash";
        sha256 = "106wrn2wspci19a70006g5xsh679ap2973h2lmssf5xbl3r3lv7g";
      };
    in ''
      # load git-prompt script
      . ${gitPrompt}
      # load git completions
      . ${gitCompletion}

      PS1='\[\e[32m\]\u\[\e[0m\]@\[\e[38;5;126m\]\h\[\e[0m\] \[\e[38;5;40m\]\w\[\e[38;5;147m\]$(__git_ps1 " (%s)")\[\e[0m\]> '
    '';
  };
}
