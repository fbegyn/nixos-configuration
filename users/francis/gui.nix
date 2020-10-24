{ config, pkgs, ... }:

{
  imports = [
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
    ./configurations/spotifyd.nix
    ./configurations/mail
    ./qalculate.nix
  ];

  home-manager.users.francis = {
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
    ];
  };
}
