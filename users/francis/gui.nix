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
      ./hm/configurations/nvim/default.nix
      ./hm/configurations/fzf.nix
      ./hm/go.nix
      ./hm/python.nix
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
      ./hm/configurations/qutebrowser
      ./secrets/fish.nix
    ];

    xdg.configFile = {
      # "qutebrowser/config.py".source = ./hm/configurations/qutebrowser/config.py;
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
    };

    home.packages = with pkgs.unstable; [
      ripgrep
      fd
      inetutils
      pciutils
      usbutils
      niv
      nodejs
      vault
      gcc
      deploy-rs
      # Comms
      slack
      mattermost-desktop
      tdesktop
      thunderbird
      elixir
      flyctl
      weechat
      # Browser
      firefox
      chromium
      # entertainement
      playerctl
      # Utilities
      # comma
      libnotify
      lm_sensors
      vagrant
      terraform
      terraform-lsp
      ansible
      gnumake
      bat
      tig
      sshuttle
      pulsemixer
      bitwarden
      bitwarden-cli
      gnome.nautilus
      dmenu
      solaar
      morph
      evince
      okular
      pulsemixer
      pandoc
      texlive.combined.scheme-small
      pkgs.pgcli
      pkgs.mycli
      pkgs.litecli
      sqlite
      # cachix
      cachix
      # tools rewritten in rust
      hyperfine
      bandwhich
      # unfree packages
      spotify
      discord
      (vscode-with-extensions.override {
        vscodeExtensions = with vscode-extensions; [
          bbenoist.nix
          vscodevim.vim
          ms-vscode-remote.remote-ssh
          ms-azuretools.vscode-docker
          ms-vsliveshare.vsliveshare
          golang.go
          ms-python.vscode-pylance
          coolbear.systemd-unit-file
          redhat.vscode-yaml
        ];
      })
    ];
  };
}
