{ config, pkgs, ... }:
{
  services.gvfs.enable = true;
  services.gnome.sushi.enable = true;
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];

  programs = {
    wireshark = {
      enable = true;
      package = pkgs.unstable.wireshark;
    };
  };

  home-manager.users.francis = {
    imports = [
      ./hm/go.nix
      ./hm/configurations/mpv
      ./hm/configurations/emacs
      ./hm/configurations/newsboat.nix
      ./hm/configurations/direnv.nix
      ./hm/configurations/zathura.nix
      ./hm/configurations/udiskie.nix
      ./hm/configurations/alacritty
      ./hm/configurations/hledger.nix
      ./hm/configurations/tmux
      ./hm/configurations/josm.nix
      ./hm/configurations/udiskie.nix
      ./hm/configurations/qutebrowser
      #./secrets/fish.nix
    ];

    xdg.configFile = {
      # "qutebrowser/config.py".source = ./hm/configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
    };

    home.packages = with pkgs; [
      niv
      unstable.nodejs
      # Comms
      slack
      unstable.mattermost-desktop
      unstable.tdesktop
      unstable.thunderbird
      unstable.elixir
      unstable.flyctl
      unstable.weechat
      # Browser
      unstable.firefox
      unstable.chromium
      # entertainement
      unstable.playerctl
      # Utilities
      # comma
      pulsemixer
      unstable.bitwarden
      unstable.bitwarden-cli
      unstable.gnome.nautilus
      unstable.dmenu
      unstable.solaar
      unstable.morph
      unstable.evince
      unstable.okular
      pulsemixer
      unstable.pandoc
      wkhtmltopdf
      texlive.combined.scheme-small
      unstable.pgcli
      unstable.mycli
      unstable.litecli
      unstable.sqlite
      # cachix
      cachix
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
