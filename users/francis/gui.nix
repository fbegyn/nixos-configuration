{ config, pkgs, ... }:

{
  imports = [
    ./configurations/mpv
    ./configurations/newsboat
    ./configurations/redshift
    ./configurations/direnv
    ./configurations/zathura
    ./configurations/udiskie
    ./configurations/alacritty
    ./configurations/ledger
    ./configurations/teamspeak.nix
    ./configurations/neuron.nix
    ./configurations/nvim/default.nix
    ./qalculate.nix
  ];

  home-manager.users.francis = {
    xsession.initExtra = ''
      systemctl --user import-environment
    '';
    services = {
      udiskie = {
        enable = true;
        automount = false;
      };
      unclutter = {
        enable = true;
        timeout = 5;
      };
    };
    xdg.configFile = {
      "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./configurations/qutebrowser/solarized-dark-all-sites.css;
      "compton.conf".source = ./configurations/compton.conf;
    };

    programs.go.enable = true;

    home.packages = with pkgs; [
      nodejs
      niv
      fzf
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
      ledger
      hledger
      gimp
      ltunify
      gnome3.nautilus
      rofi
      rofi-pass
      unstable.vscode
      pavucontrol
      pulsemixer
      libnotify
      unstable.pandoc
      texlive.combined.scheme-medium
      wkhtmltopdf
      unstable.reflex
    ];
  };
}
