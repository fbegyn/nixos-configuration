{ config, pkgs, ... }:

let
  comma = import (builtins.fetchTarball "https://github.com/Shopify/comma/archive/60a4cf8ec5c93104d3cfb9fc5a5bac8fb18cc8e4.tar.gz") { inherit pkgs; };
in {
  imports = [
    ./python.nix
    ./configurations/mpv
    ./configurations/newsboat.nix
    ./configurations/redshift.nix
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
  };
  services = {
    unclutter = {
      enable = true;
      timeout = 5;
    };
  };

  programs.go.enable = true;
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    nodejs
    niv
    unstable.jq
    # Comms
    slack
    mattermost-desktop
    # Browser
    unstable.firefox
    unstable.qutebrowser
    tdesktop
    # entertainement
    spotify
    unstable.playerctl
    # Utilities
    go-tools
    gimp
    ltunify
    gnome3.nautilus
    rofi
    rofi-pass
    unstable.vscode
    unstable.evince
    pavucontrol
    pulsemixer
    libnotify
    unstable.pandoc
    texlive.combined.scheme-medium
    wkhtmltopdf
    unstable.reflex
    unstable.thunderbird-78
    libqalculate
    nixops
    unstable.htop
    iftop
    clang
    llvm
    units
    lm_sensors
    neofetch
    moreutils
    screen
    pass
    inotify-tools
    gnumake
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

    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
      unstable = import (builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz") {
          config = config.nixpkgs.config;
        };
    };
  };
}
