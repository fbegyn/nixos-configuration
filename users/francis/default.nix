{ config, pkgs, ... }:

let
  comma = import (pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "comma";
    rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
    sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) { };
in {
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./python.nix
    ./configurations/git.nix
    ./configurations/tmux
    ./configurations/fish.nix
    ./configurations/fzf.nix
    ./configurations/josm.nix
    ../../common/unstable.nix
    ./ssh.nix
  ];

  home-manager.users.francis = {
    home.packages = with pkgs; [
      nixops
      unstable.htop
      iftop
      clang
      llvm
      units
      lm_sensors
      neofetch
      moreutils
      screen
      pass
      inotify-tools
      gnumake
      whois
      # cachix
      cachix
      # tools rewritten in rust
      unstable.ripgrep
      unstable.fd
      unstable.tmux
      unstable.hyperfine
      unstable.bandwhich
      unzip
      # run nix programs once without installing
      comma
    ];

    programs.home-manager.enable = true;
  };

  virtualisation.docker.enable = true;
}
