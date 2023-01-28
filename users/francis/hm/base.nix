{ config, pkgs, ... }:

{
  imports = [
    ./configurations/nvim/default.nix
    ./configurations/git.nix
    ./configurations/fish.nix
    ./configurations/bash.nix
    ./configurations/starship.nix
    ./configurations/fzf.nix
  ];

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./configurations/nixpkgs-config.nix;
  };

  programs.home-manager.enable = true;

  home.sessionPath = [
    "$HOME/go/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };
  home.sessionVariables = { EDITOR = "vim"; };

  home.packages = with pkgs.unstable; [
    # Utilities
    jq
    yq
    killall
    htop
    rclone
    moreutils
    screen
    inotify-tools
    # tools rewritten in rust
    ripgrep
    fd
    unzip
    envsubst
    wget
    inetutils
    pciutils
    usbutils
  ];
}
