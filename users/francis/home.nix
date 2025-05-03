{ config, pkgs, ... }:

{
  home.stateVersion = "24.11";

  programs.ghostty = {
    enable = false;
  };

  imports = [
    ./hm/base.nix
    ./hm/colors.nix
    ./hm/go.nix
    ./hm/python.nix
    ./hm/configurations/git.nix
    ./hm/configurations/bash.nix
    ./hm/configurations/fish.nix
    ./hm/configurations/fzf.nix
    ./hm/configurations/emacs
    ./hm/configurations/nvim
    ./hm/configurations/mpv
    ./hm/configurations/direnv.nix
    ./hm/configurations/zathura.nix
    ./hm/configurations/udiskie.nix
    ./hm/configurations/alacritty
    ./hm/configurations/tmux
    ./hm/configurations/redshift.nix
    ./hm/configurations/qutebrowser
  ];

  xdg.configFile = {
    "qutebrowser/css/solarized-dark-all-sites.css".source =
      ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
  };

  home.packages = with pkgs.unstable; [
    ripgrep
    projecteur
    fd
    inetutils
    pciutils
    usbutils
    # nodejs
    gcc
    (aspellWithDicts (dicts: with dicts; [en en-science nl]))
    thunderbird
    # Browser
    chromium
    firefox
    # entertainement
    playerctl
    pulsemixer
    # Utilities
    lm_sensors
    gnumake
    ansible
    sshpass
    talosctl
    bat
    tig
    sshuttle
    unzip
    dmenu
    pandoc
    texlive.combined.scheme-small
    pkgs.pgcli
    pkgs.mycli
    pkgs.litecli
    sqlite
    nix-index
    # tools rewritten in rust
    hyperfine
  ];
}
