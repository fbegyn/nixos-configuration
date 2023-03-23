{ config, pkgs, ... }:

{
  imports = [
    ./configurations/git.nix
    ./configurations/fish
    ./configurations/bash.nix
  ];

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./configurations/nixpkgs-config.nix;
  };

  programs.home-manager.enable = true;
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
  };

  home.sessionPath = [
    "$HOME/go/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };
  home.sessionVariables = { EDITOR = "vim"; };

  home.stateVersion = "22.11";

  home.packages = with pkgs.unstable; [
    # Utilities
    openssl
    jq
    yq
    killall
    htop
    rclone
    moreutils
    screen
    inotify-tools
    envsubst
    wget
  ];
}
