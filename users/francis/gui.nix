{ config, pkgs, ... }:
{
  services.gvfs.enable = true;
  services.gnome.sushi.enable = true;
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];
  services.gnome.gnome-keyring.enable = true;

  programs = {
    wireshark = {
      enable = true;
      package = pkgs.unstable.wireshark;
    };
  };
  environment.systemPackages = with pkgs; [ firefox ];

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
      # ./hm/configurations/qutebrowser
      ./secrets/fish.nix
    ];

    xdg.configFile = {
      "qutebrowser/css/solarized-dark-all-sites.css".source =
        ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
    };

    home.packages = with pkgs.unstable; [
      ripgrep
      fd
      inetutils
      pciutils
      usbutils
      nodejs
      gcc
      (aspellWithDicts (dicts: with dicts; [en en-science nl]))
      # Comms
      slack
      mattermost-desktop
      thunderbird
      weechat
      # Browser
      chromium
      # entertainement
      playerctl
      pulsemixer
      # Utilities
      flyctl
      lm_sensors
      gnumake
      ansible
      bat
      tig
      sshuttle
      unzip
      #bitwarden
      bitwarden-cli
      bitwarden
      rbw
      nautilus
      dmenu
      solaar
      evince
      okular
      pandoc
      texlive.combined.scheme-small
      pkgs.pgcli
      pkgs.mycli
      pkgs.litecli
      sqlite
      # cachix
      cachix
      nix-index
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
          elixir-lsp.vscode-elixir-ls
          ms-vscode-remote.remote-ssh
          ms-azuretools.vscode-docker
          ms-vsliveshare.vsliveshare
          golang.go
          coolbear.systemd-unit-file
          redhat.vscode-yaml
        ];
      })
    ];
  };
}
