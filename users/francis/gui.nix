{ config, pkgs, ... }:
let
  comma = import (builtins.fetchTarball "https://github.com/nix-community/comma/archive/refs/heads/master.tar.gz") { inherit pkgs; };
in {
  services.gvfs.enable = true;

  programs = {
    wireshark = {
      enable = true;
      package = pkgs.unstable.wireshark;
    };
  };

  home-manager.users.francis = {
    imports = [
      ./go.nix
      ./configurations/mpv
      ./configurations/newsboat.nix
      ./configurations/direnv.nix
      ./configurations/zathura.nix
      ./configurations/udiskie.nix
      ./configurations/alacritty
      ./configurations/hledger.nix
      ./configurations/tmux
      ./configurations/josm.nix
      ./configurations/emacs
      ./configurations/udiskie.nix
      #./secrets/fish.nix
    ];

    xdg.configFile = {
      "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./configurations/qutebrowser/solarized-dark-all-sites.css;
    };

    home.packages = with pkgs; [
      niv
      unstable.nodejs
      # Comms
      slack
      unstable.mattermost-desktop
      unstable.tdesktop
      unstable.thunderbird
      unstable.weechat
      # Browser
      unstable.firefox
      unstable.chromium
      unstable.qutebrowser
      # entertainement
      unstable.playerctl
      # Utilities
      comma
      pulsemixer
      unstable.bitwarden
      unstable.bitwarden-cli
      gnome3.nautilus
      unstable.dmenu
      unstable.solaar
      unstable.morph
      unstable.evince
      unstable.okular
      pulsemixer
      unstable.pandoc
      wkhtmltopdf
      texlive.combined.scheme-small
      unstable.tmux
      # cachix
      cachix
      # pass # no longer needed since handled by pass-otp
      pass-otp
      # tools rewritten in rust
      unstable.hyperfine
      unstable.bandwhich
      # unfree packages
      unstable.spotify
      unstable.vscode
      unstable.discord
    ];
  };
}
