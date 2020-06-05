{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./python.nix
    ./configurations/git.nix
    ./configurations/tmux
  ];

  home-manager.users.francis = { pkgs , ... }: {
    home.packages = with pkgs; [
      htop
      iftop
      lm_sensors
      neofetch
      moreutils
      pass
      ripgrep
      tmux
      unzip
    ];
  };

  virtualisation.docker.enable = true;
}
