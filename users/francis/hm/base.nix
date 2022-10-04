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

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = import ../../../pkgs;
  };

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

  home.sessionVariables = { EDITOR = "vim"; };

  home.packages = with pkgs; [
    unstable.jq
    # Utilities
    gcc
    libnotify
    libqalculate
    unstable.htop
    unstable.rclone
    lm_sensors
    moreutils
    screen
    unstable.tmux
    inotify-tools
    gnumake
    # tools rewritten in rust
    unstable.ripgrep
    unstable.fd
    unzip
    envsubst
    wget
    inetutils
    pciutils
    usbutils
  ];
}
