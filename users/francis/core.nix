{ config, pkgs, ... }:

let
  comma = import (builtins.fetchTarball "https://github.com/Shopify/comma/archive/60a4cf8ec5c93104d3cfb9fc5a5bac8fb18cc8e4.tar.gz") { inherit pkgs; };
in {
  imports = [
    ./python.nix
    ./configurations/udiskie.nix
    ./configurations/nvim/default.nix
    ./configurations/git.nix
    ./configurations/fish.nix
    ./configurations/starship.nix
    ./configurations/fzf.nix
    ./configurations/network-tools.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = import ../../pkgs;
  };

  xsession.initExtra = ''
    systemctl --user import-environment
  '';

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./configurations/nixpkgs-config.nix;
  };

  programs.go = {
    enable = true;
    goPath = "go";
  };

  programs.home-manager.enable = true;

  home.sessionPath = [
    "$HOME/go/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
  ];

  home.sessionVariables = { EDITOR = "vim"; };

  home.packages = with pkgs; [
    niv
    unstable.jq
    # Utilities
    gcc
    go-tools
    pulsemixer
    libnotify
    libqalculate
    unstable.htop
    unstable.rclone
    lm_sensors
    moreutils
    screen
    unstable.tmux
    unstable.morph
    inotify-tools
    gnumake
    # cachix
    cachix
    # tools rewritten in rust
    unstable.ripgrep
    unstable.fd
    unstable.hyperfine
    unstable.bandwhich
    unzip
    # run nix programs once without installing
    comma
  ];
}
