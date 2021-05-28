{ config, pkgs, ... }:

{
  home-manager.users.francis = {
    imports = [
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
      #./secrets/fish.nix
    ];

    xdg.configFile = {
      "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./configurations/qutebrowser/solarized-dark-all-sites.css;
      "mimeapps.list".source = ./configurations/mimeapps.list;
    };

    home.packages = with pkgs; [
      unstable.nodejs
      # Comms
      slack
      unstable.mattermost-desktop
      unstable.tdesktop
      unstable.thunderbird
      # Browser
      unstable.firefox
      unstable.chromium
      unstable.qutebrowser
      # entertainement
      unstable.playerctl
      # Utilities
      gnome3.nautilus
      unstable.dmenu
      unstable.ltunify
      unstable.evince
      pulsemixer
      unstable.pandoc
      wkhtmltopdf
      texlive.combined.scheme-small
      # pass # no longer needed since handled by pass-otp
      pass-otp
      # tools rewritten in rust
      unstable.tmux
      unstable.hyperfine
      unstable.bandwhich
      # unfree packages
      unstable.spotify
      unstable.vscode
      unstable.discord
    ];
  };
}
