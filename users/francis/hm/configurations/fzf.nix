{ pkgs, ... }:

{
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;
    fileWidgetCommand = "fd --hidden --strip-cwd-prefix --exclude .git";
  };
}
