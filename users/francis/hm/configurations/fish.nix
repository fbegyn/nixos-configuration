{ config, pkgs, ... }: {
  programs.autojump.enable = true;
  programs.fish = {
    enable = true;
    package = pkgs.fish;
    shellAliases = {
      "vim" = "nvim";
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
      "gch" = "git checkout";
      "k" = "kubectl";
      "gitr" = "git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5";
      "dgitr" = "bash -c 'for d in ./*/; do (echo $d && cd $d && git pull --rebase; git submodule --quiet sync; git submodule update --init --recursive --jobs 5); done'";
    };
    interactiveShellInit = ''
      set -Ux BROWSER firefox
      # wayland variables
      set -Uxa XDG_SESSION_TYPE wayland
      set -Uxa QT_WAYLAND_DISABLE_WINDOWDECORATION 1
      set -Uxa QT_AUTO_SCREEN_SCALE_FACTOR 0
      set -Uxa QT_SCALE_FACTOR 1
      set -Uxa GDK_SCALE 1
      set -Uxa GDK_DPI_SCALE 1
      set -Uxa MOZ_ENABLE_WAYLAND 1
      set -Uxa _JAVA_AWT_WM_NONREPARENTING 1

      if test "$tide_config_rev" != "2"
        set -U tide_left_prompt_items pwd vcs newline character
        set -U tide_config_rev 2
      end

      # If a dumb terminal connects, just show simple prompt
      if test "$TERM" = "dumb"
        function fish_prompt
          echo "\$ "
        end
        function fish_right_prompt; end
        function fish_greeting; end
        function fish_title; end
      end
    '';
    plugins = with pkgs.unstable.fishPlugins; [
      { name = "tide"; src = tide.src; }
      { name = "fzf-fish"; src = fzf-fish.src; }
      { name = "autopair"; src = autopair.src; }
      { name = "tide-item-jj";
        src = pkgs.fetchFromGitHub {
          owner = "lucasadelino";
          repo = "tide-item-jj";
          rev = "e1150b7332b85149b468cb10c2844f082f33975b";
          hash = "sha256-vLSrHPoytZ/kXQh0Bp/4AWe8YLlyufRjepfXUAuWCB8=";
        };
      }
    ];
  };
}
