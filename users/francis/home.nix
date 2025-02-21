{ config, pkgs, ... }:

{
  home.stateVersion = "24.11";

  programs.ghostty = {
    enable = false;
  };

  imports = [
    ./hm/base.nix
    ./hm/colors.nix
    ./hm/go.nix
    ./hm/python.nix
    ./hm/configurations/git.nix
    ./hm/configurations/bash.nix
    ./hm/configurations/fish.nix
    ./hm/configurations/fzf.nix
    ./hm/configurations/emacs
    ./hm/configurations/nvim
    ./hm/configurations/mpv
    ./hm/configurations/direnv.nix
    ./hm/configurations/zathura.nix
    ./hm/configurations/udiskie.nix
    ./hm/configurations/alacritty
    ./hm/configurations/tmux
    ./hm/configurations/redshift.nix
    ./hm/configurations/qutebrowser
  ];

  xdg.configFile = {
    "qutebrowser/css/solarized-dark-all-sites.css".source =
      ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
  };

  home.packages = with pkgs.unstable; [
    ripgrep
    projecteur
    fd
    inetutils
    pciutils
    usbutils
    nodejs
    gcc
    (aspellWithDicts (dicts: with dicts; [en en-science nl]))
    thunderbird
    # Browser
    chromium
    firefox
    # entertainement
    playerctl
    pulsemixer
    # Utilities
    lm_sensors
    gnumake
    ansible
    bat
    tig
    sshuttle
    unzip
    pkgs.bitwarden-cli
    pkgs.bitwarden
    keepassxc
    rbw
    nautilus
    dmenu
    pkgs.solaar
    evince
    okular
    pandoc
    texlive.combined.scheme-small
    pkgs.pgcli
    pkgs.mycli
    pkgs.litecli
    sqlite
    nix-index
    # tools rewritten in rust
    hyperfine
    # unfree packages
    spotify
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

  nix = {
    settings = {
      sandbox = true;
      substituters = [
        "https://fbegyn-personal.cachix.org/"
        "https://nix-community.cachix.org/"
        "https://cuda-maintainers.cachix.org/"
        "https://cache.lix.systems/"
        "https://cache.flox.dev/"
      ];
      trusted-public-keys = [
        "fbegyn-personal.cachix.org-1:0BEArpeI+ISsPainphPLHBozpP+zExYO6+43lLORDnI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
      ];
      trusted-users = [ "root" "fbegyn" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '' + pkgs.lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
  };
}
