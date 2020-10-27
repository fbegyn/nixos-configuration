{ config, pkgs, ... }:

let
  comma = import (builtins.fetchTarball "https://github.com/Shopify/comma/archive/60a4cf8ec5c93104d3cfb9fc5a5bac8fb18cc8e4.tar.gz") { inherit pkgs; };
in {
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./python.nix
    ./configurations/git.nix
    ./configurations/tmux
    ./configurations/fish.nix
    ./configurations/starship.nix
    ./configurations/fzf.nix
    ./configurations/josm.nix
    ./configurations/network-tools.nix
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
  virtualisation.podman.enable = true;
}
