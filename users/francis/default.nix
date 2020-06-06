{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./python.nix
    ./configurations/git.nix
    ./configurations/tmux
  ];

  home-manager.users.francis = {
    home.packages = with pkgs; [
      htop
      iftop
      lm_sensors
      neofetch
      moreutils
      pass
      ripgrep
      fd
      tmux
      unzip
    ];

    programs.home-manager.enable = true;
  };

  virtualisation.docker.enable = true;
}
