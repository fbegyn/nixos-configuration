{ config, pkgs, ... }:

{
  imports = [
    ./python.nix
    ./configurations/nvim/default.nix
    ./configurations/git.nix
    ./configurations/fish.nix
    ./configurations/bash.nix
    ./configurations/starship.nix
    ./configurations/fzf.nix
  ];

  xsession.initExtra = ''
    systemctl --user import-environment
  '';

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
    gcc
    libnotify
    libqalculate
    htop
    rclone
    lm_sensors
    moreutils
    screen
    tmux
    inotify-tools
    gnumake
    # tools rewritten in rust
    ripgrep
    fd
    unzip
    envsubst
    wget
    httpie
    inetutils
    pciutils
    usbutils
  ];
}
