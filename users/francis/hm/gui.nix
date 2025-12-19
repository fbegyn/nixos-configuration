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
    slack
    mattermost-desktop
    discord
    spotify
    meld
    cachix
    projecteur
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
