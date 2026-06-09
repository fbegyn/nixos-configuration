{ pkgs, ... }:
{
  imports = [
    ./configurations/nvim
    ./configurations/fzf.nix
    ./go.nix
    ./python.nix
    ./configurations/mpv
    ./configurations/emacs
    ./configurations/vscode
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
    cue
    discord
    spotify
    meld
    cachix
    projecteur
    vdirsyncer
    khard
  ];
}
