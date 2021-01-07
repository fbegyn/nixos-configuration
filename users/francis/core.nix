{ config, pkgs, ... }:

let
  comma = import (builtins.fetchTarball "https://github.com/Shopify/comma/archive/60a4cf8ec5c93104d3cfb9fc5a5bac8fb18cc8e4.tar.gz") { inherit pkgs; };
in {
  imports = [
    ./python.nix
    ../../common/unstable.nix
    ../../common/nur.nix
    ../../common/master.nix
    ./configurations/mpv
    ./configurations/newsboat.nix
    ./configurations/direnv.nix
    ./configurations/zathura.nix
    ./configurations/udiskie.nix
    ./configurations/alacritty
    ./configurations/hledger.nix
    ./configurations/teamspeak.nix
    ./configurations/neuron.nix
    ./configurations/nvim/default.nix
    ./configurations/emacs/default.nix
    ./configurations/spotifyd.nix
    ./configurations/git.nix
    ./configurations/tmux
    ./configurations/fish.nix
    ./configurations/starship.nix
    ./configurations/fzf.nix
    ./configurations/josm.nix
    ./configurations/network-tools.nix
    ./secrets/fish.nix
  ];

  xsession.initExtra = ''
    systemctl --user import-environment
  '';

  xdg.configFile = {
    "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
    "qutebrowser/css/solarized-dark-all-sites.css".source =
      ./configurations/qutebrowser/solarized-dark-all-sites.css;
    "compton.conf".source = ./configurations/compton.conf;
    "mimeapps.list".source = ./configurations/mimeapps.list;
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

  home.packages = with pkgs; [
    nodejs
    niv
    unstable.jq
    # Comms
    slack
    master.mattermost-desktop
    master.discord
    tdesktop
    # Browser
    unstable.firefox
    unstable.qutebrowser
    # entertainement
    spotify
    unstable.playerctl
    # Utilities
    go-tools
    gnome3.nautilus
    rofi
    rofi-pass
    unstable.ltunify
    unstable.vscode
    unstable.evince
    pulsemixer
    libnotify
    unstable.pandoc
    texlive.combined.scheme-medium
    unstable.reflex
    unstable.thunderbird-78
    libqalculate
    unstable.nixops
    unstable.htop
    llvm
    lm_sensors
    moreutils
    screen
    pass
    inotify-tools
    gnumake
    linuxPackages.bcc
    whois
    # cachix
    cachix
    # tools rewritten in rust
    unstable.ripgrep
    unstable.fd
    unstable.tmux
    unstable.hyperfine
    unstable.bandwhich
    unzip
    # run nix programs once without installing
    comma
  ];

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };
}
