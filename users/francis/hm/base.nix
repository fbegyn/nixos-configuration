{ config, pkgs, ... }:

{
  imports = [
    ./colors.nix
    ./configurations/git.nix
    ./configurations/bash.nix
    ./configurations/fish.nix
  ];

  programs.home-manager.enable = true;
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    defaultEditor = false;
  };

  home.sessionPath = [
    "$HOME/.go/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
  ];

  home.packages = with pkgs.unstable; [
    findutils
    # Utilities
    openssl
    jq
    yq
    killall
    htop
    rclone
    moreutils
    screen
    envsubst
    wget
    curl
  ];
}
