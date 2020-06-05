{ pkgs, ... }:

{
  home-manager.users.francis.home.file = {
    ".tmux.conf".source = ./tmux.conf;
    ".tmux/yank.sh".source = ./yank.sh;
    ".tmux/renew_env.sh".source = ./renew_env.sh;
  };
}
