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
      ./configurations/neuron.nix
      ./configurations/spotifyd.nix
      ./configurations/tmux
      ./configurations/josm.nix
      ./configurations/emacs
      #./configurations/spacemacs
      #./secrets/fish.nix
    ];

    xdg.configFile = {
      "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./configurations/qutebrowser/solarized-dark-all-sites.css;
      "compton.conf".source = ./configurations/compton.conf;
      "mimeapps.list".source = ./configurations/mimeapps.list;
    };

    home.packages = with pkgs; [
      nodejs
      # Comms
      slack
      unstable.mattermost-desktop
      master.discord
      unstable.tdesktop
      # Browser
      firefox
      unstable.qutebrowser
      # entertainement
      spotify
      unstable.playerctl
      # Utilities
      gnome3.nautilus
      rofi
      rofi-pass
      unstable.ltunify
      # unstable.vscode
      unstable.evince
      pulsemixer
      unstable.pandoc
      wkhtmltopdf
      texlive.combined.scheme-small
      unstable.thunderbird-78
      pass
      # tools rewritten in rust
      unstable.tmux
      unstable.hyperfine
      unstable.bandwhich
    ];
  };
}
