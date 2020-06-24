{ config, pkgs, ... }:

let
    comma = import ( pkgs.fetchFromGitHub {
      owner = "Shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) {};
in
{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./python.nix
    ./configurations/git.nix
    ./configurations/tmux
    ./configurations/fish
    ./ssh.nix
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
      comma
      inotify-tools
    ];

    programs.home-manager.enable = true;
  };

  virtualisation.docker.enable = true;
}
