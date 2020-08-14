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

    programs.go.enable = true;

    home.packages = let
      customnvim = pkgs.neovim.override {
        # don't alias neovim to vim, yet.
        vimAlias = true;

        configure = (import ./configurations/nvim/customization.nix { pkgs = pkgs; });
      };
    in
      with pkgs; [
        niv
        fzf
        # Comms
        slack
        mattermost-desktop
        # Browser
        unstable.firefox
        qutebrowser
        tdesktop
        # entertainement
        spotify
        unstable.playerctl
        # Utilities
        gimp
        ltunify
        customnvim
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
        reflex
      ];
  };
}
