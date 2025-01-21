{ config, pkgs, ... }:

{
  imports = [
    ./hm/colors.nix
    ./hm/configurations/git.nix
    ./hm/configurations/bash.nix
    ./hm/configurations/fish.nix
  ];

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./hm/configurations/nixpkgs-config.nix;
  };

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

  nixpkgs.config = {
    allowUnfree = true;
  };

  home.packages = with pkgs.unstable; [
    findutils
    home-manager
    # Utilities
    jujutsu
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
