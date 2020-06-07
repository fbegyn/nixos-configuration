{ config, pkgs, ... }:

{
  imports = [
    ./configurations/i3
    ./configurations/dunst
    ./configurations/mpv
    ./configurations/newsboat
    ./configurations/redshift
    ./configurations/direnv
    ./configurations/mail
  ];

  home-manager.users.francis = {
    xsession.initExtra = ''
      systemctl --user import-environment
    '';
    services.lorri.enable = true;
    home.file = {
      ".config/qutebrowser/config.py".source = ./configurations/qutebrowser/config.py;
      ".config/compton.conf".source = ./configurations/compton.conf;
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
        unzip
        # Comms
        slack
        mattermost-desktop
        # Browser
        firefox
        qutebrowser
        # entertainement
        spotify
        # Utilities
        customnvim
        gnome3.nautilus
        rofi
        rofi-pass
        vscode
        youtube-dl
        streamlink
      ];
  };
}
