{ config, pkgs, ... }:
let
  comma = import (builtins.fetchTarball "https://github.com/nix-community/comma/archive/refs/heads/master.tar.gz") { inherit pkgs; };
in {
  services.gvfs.enable = true;
  services.gnome.sushi.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

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
      ./hm/configurations/newsboat.nix
      ./hm/configurations/direnv.nix
      ./hm/configurations/zathura.nix
      ./hm/configurations/udiskie.nix
      ./hm/configurations/alacritty
      ./hm/configurations/hledger.nix
      ./hm/configurations/tmux
      ./hm/configurations/josm.nix
      ./hm/configurations/emacs
      ./hm/configurations/udiskie.nix
      #./secrets/fish.nix
    ];

    xdg.configFile = {
      "qutebrowser/config.py".source = ./hm/configurations/qutebrowser/config.py;
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
      thunderbird
      unstable.weechat
      # Browser
      unstable.firefox
      unstable.chromium
      qutebrowser
      # entertainement
      unstable.playerctl
      # Utilities
      comma
      pulsemixer
      unstable.bitwarden
      unstable.bitwarden-cli
      unstable.gnome3.nautilus
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
