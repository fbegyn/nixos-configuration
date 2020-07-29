{ config, pkgs, ... }:

{
  imports = [
    ./configurations/mpv
    ./configurations/newsboat
    ./configurations/redshift
    ./configurations/direnv
    ./configurations/mail
    ./configurations/zathura
    ./configurations/udiskie
    ./configurations/alacritty
    ./configurations/ledger
    ./qalculate.nix
  ];

  home-manager.users.francis = {
    xsession.initExtra = ''
      systemctl --user import-environment
    '';
    services.lorri.enable = true;
    xdg.configFile = {
      "qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source = ./configurations/qutebrowser/solarized-dark-all-sites.css;
      "compton.conf".source = ./configurations/compton.conf;
    };

    home.packages = let
      customnvim = pkgs.neovim.override {
        # don't alias neovim to vim, yet.
        vimAlias = true;

        configure = (import ./configurations/nvim/customization.nix { pkgs = pkgs; });
      };
    in
      with pkgs; [
        fzf
        alacritty
        unzip
        # Comms
        slack
        mattermost-desktop
        # Browser
        firefox
        qutebrowser
        # entertainement
        spotify
        playerctl
        # Utilities
        gimp
        ltunify
        customnvim
        gnome3.nautilus
        rofi
        rofi-pass
        vscode
        youtube-dl
        streamlink
        pavucontrol
        pulsemixer
        libnotify
        pandoc
        texlive.combined.scheme-medium
        wkhtmltopdf
        reflex
        teams
      ];
  };
}
