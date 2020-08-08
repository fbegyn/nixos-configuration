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
    ./configurations/josm.nix
    ../../common/unstable.nix
    ./ssh.nix
  ];

  home-manager.users.francis = {
    home.packages = with pkgs; [
      nixops
      htop
      iftop
      units
      lm_sensors
      neofetch
      moreutils
      screen
      pass
      ripgrep
      fd
      unstable.tmux
      unzip
      comma
      inotify-tools
      gnumake
      whois
    ];

    programs.home-manager.enable = true;
  };

  virtualisation.docker.enable = true;
}
