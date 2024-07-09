{ config, pkgs, nix-colors, ... }:

{
  imports = [
    ./configurations/git.nix
    ./configurations/fish
    ./configurations/bash.nix
    nix-colors.homeManagerModules.default
  ];

  # colorScheme = let
  #   colorscheme = "base16/gruvbox-light-medium";
  #   src =
  #     "https://raw.githubusercontent.com/tinted-theming/schemes/spec-0.11/${colorscheme}.yaml";
  #   theme = nix-colors.lib.schemeFromYAML builtins.readFile ( builtins.fetchurl {
  #     url = "${src}";
  #     sha256 = "sha256:1gqnn8wpjwqyn33r3s69xgappgf7z76wlnf4asi4yn4l32a5z4ks";
  #   });
  # in theme;

  colorScheme = nix-colors.colorSchemes.base16.gruvbox-material-dark-hard;

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./configurations/nixpkgs-config.nix;
  };

  programs.home-manager.enable = true;
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    defaultEditor = true;
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
