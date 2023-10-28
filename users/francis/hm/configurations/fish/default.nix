{ pkgs, ...}:

{
  # home.file = {
  #   ".config/fish/functions/fish_prompt.fish".source = ./fish_prompt.fish;
  #   ".config/fish/functions/fish_right_prompt.fish".source = ./fish_right_prompt.fish;
  # };
  programs.fish = {
    enable = true;
    package = pkgs.unstable.fish;
    shellAliases = {
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
      "gch" = "git checkout";
      "k" = "kubectl";
    };
    loginShellInit = ''
      set -Ux BROWSER chromium-browser
      # wayland variables
      set -Uxa XDG_SESSION_TYPE wayland
      set -Uxa QT_WAYLAND_DISABLE_WINDOWDECORATION 1
      set -Uxa QT_AUTO_SCREEN_SCALE_FACTOR 0
      set -Uxa QT_SCALE_FACTOR 1
      set -Uxa GDK_SCALE 1
      set -Uxa GDK_DPI_SCALE 1
      set -Uxa MOZ_ENABLE_WAYLAND 1
      set -Uxa _JAVA_AWT_WM_NONREPARENTING 1

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
  };
}
