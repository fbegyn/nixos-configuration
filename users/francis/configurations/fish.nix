{ pkgs, ...}:

{
  imports = [
    ../secrets/fish.nix
  ];
  programs.fish = {
    enable = true;
    package = pkgs.unstable.fish;
    shellAliases = {
      "gst" = "git status";
      "ga" = "git add";
      "glg" = "git log";
      "gc" = "git commit";
      "gcmsg" = "git commit -m";
    };
    loginShellInit = ''
      set -Ux SSH_AUTH_SOCK /run/user/1000/ssh-agent
      set -Ux BROWSER qutebrowser
      # wayland variables
      set -Uxa XDG_SESSION_TYPE wayland
      set -Uxa QT_WAYLAND_DISABLE_WINDOWDECORATION 1
      set -Uxa QT_AUTO_SCREEN_SCALE_FACTOR 0
      set -Uxa QT_SCALE_FACTOR 1
      set -Uxa GDK_SCALE 1
      set -Uxa GDK_DPI_SCALE 1
      set -Uxa MOZ_ENABLE_WAYLAND 1
      set -Uxa _JAVA_AWT_WM_NONREPARENTING 1
    '';
    plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
          sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
        };
      }
      {
        name = "fzf";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "fzf";
          rev = "24f4739fc1dffafcc0da3ccfbbd14d9c7d31827a";
          sha256 = "0kz057nr07ybh0y06ww3p424rgk8pi84pnch9jzb040qqn9a8823";
        };
      }
    ];
  };
  
}
