{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    package = pkgs.unstable.tmux;
    sensibleOnTop = true;
    newSession = true;
    secureSocket = true;

    prefix = "C-a";
    extraConfig = let
      conf = builtins.readFile ./tmux.conf;
    in ''
      ${conf}
    '';
    plugins = with pkgs.unstable; [
      tmuxPlugins.tmux-fzf
      tmuxPlugins.prefix-highlight
    ];
  };
  home.file = {
    ".tmux.conf".source = ./tmux.conf;
    ".tmux/tmux.remote.conf".source = ./tmux.remote.conf;
    ".tmux/yank.sh".source = ./yank.sh;
    ".tmux/renew_env.sh".source = ./renew_env.sh;
  };
}
