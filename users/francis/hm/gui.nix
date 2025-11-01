{ pkgs, ... }:
{
  imports = [
    ./configurations/nvim
    ./configurations/fzf.nix
    ./go.nix
    ./python.nix
    ./configurations/mpv
    ./configurations/emacs
    ./configurations/direnv.nix
    ./configurations/zathura.nix
    ./configurations/udiskie.nix
    ./configurations/alacritty
    ./configurations/hledger.nix
    ./configurations/tmux
    ./configurations/josm.nix
    ./configurations/redshift.nix
    # ./configurations/qutebrowser
    ../secrets/bash.nix
  ];

  home.packages = with pkgs.unstable; [
    weechat
    typst
    typstfmt
    tinymist
    ripgrep
    jujutsu
    ranger
    projecteur
    fd
    inetutils
    pciutils
    usbutils
    # nodejs
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
    pkgs.bitwarden-cli
    pkgs.bitwarden
    keepassxc
    rbw
    nautilus
    dmenu
    pkgs.solaar
    evince
    kdePackages.okular
    pandoc
    texlive.combined.scheme-small
    pkgs.pgcli
    pkgs.mycli
    pkgs.litecli
    sqlite
    # cachix
    cachix
    nix-index
    meld
    nixos-generators
    # tools rewritten in rust
    hyperfine
    # unfree packages
    spotify
    discord
    ollama
    zed-editor
    (vscode-with-extensions.override {
      vscodeExtensions = with pkgs.unstable.vscode-extensions; [
        bbenoist.nix
        vscodevim.vim
        elixir-lsp.vscode-elixir-ls
        ms-vscode-remote.remote-ssh
        ms-azuretools.vscode-docker
        ms-vsliveshare.vsliveshare
        golang.go
        coolbear.systemd-unit-file
        redhat.vscode-yaml
        github.copilot
      ];
    })
  ];
}
