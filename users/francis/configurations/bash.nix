{ pkgs, ...}:

{
  programs.autojump.enable = true;
  programs.bash = {
    enable = true;
    shellAliases = {
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
    };
    sessionVariables = {
      SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
      BROWSER = "qutebrowser";
      XDG_SESSION_TYPE = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 0;
      QT_SCALE_FACTOR = 1;
      GDK_SCALE = 1;
      GDK_DPI_SCALE = 1;
      MOZ_ENABLE_WAYLAND = 1;
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
    bashrcExtra = ''
      eval "$(starship init bash)"
    '';
  };
}
