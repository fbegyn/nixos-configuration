{ config, pkgs, ... }:
{
  # system packages to install
  environment.systemPackages = with pkgs.unstable; [
    tmux
    ldns
    sshpass
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      nerdfonts
    ];
  };

  users.users.francis.home = "/Users/francis";
  environment = {
    shells = with pkgs; [
      bashInteractive
      zsh
      fish
    ];
    loginShell = pkgs.fish;
    systemPath = [
      "/Users/francis/.local/bin"
      "/Users/francis/.go/bin"
      "/opt/homebrew/bin"
    ];
  };
  services.tailscale.enable = true;
  programs.fish.enable = true;

  # darwin specific modules
  # homebrew
  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    global.brewfile = true;
    casks = [
      "docker"
      "telegram"
      "google-chrome"
      "visual-studio-code"
      "orbstack"
      "libreoffice"
      "tunnelblick"
      "secretive"
    ];
    taps = [];
    brews = [
      "htop"
    ];
  };
  # launchd config
  launchd.user.envVariables.PATH = config.environment.systemPath;
  # darwin - emacs
  launchd.user.agents.emacs.path = config.services.emacs.additionalPath ++ [ config.environment.systemPath ];
  services.emacs = {
    enable = true;
    package = config.home-manager.users.francis.emacs.package;
    additionalPath = [
      "/Users/francis/.go/bin"
      "/Users/francis/.local/bin"
    ];
  };

  # home-manager settings (darwin)
  imports = [
    ../../common/network-tools.nix
  ];
  home-manager.users.francis = {
    home.stateVersion = "23.05";
    imports = [
      ../../users/francis/hm/base.nix
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/josm.nix
      ../../users/francis/hm/configurations/fzf.nix
      ../../users/francis/hm/configurations/emacs
      ../../users/francis/hm/configurations/mpv
      ../../users/francis/hm/configurations/hledger.nix
      ../../users/francis/hm/configurations/direnv.nix
      ../../users/francis/secrets/fish.nix
    ];

    home.packages = with pkgs.unstable; [
      # Comms
      openscad
      # SRE - deployment
      flyctl
      ansible
      # Utilities
      kitty
      wakeonlan
      bitwarden-cli
      ripgrep
      fd
      bat
      discord
      inetutils
      nfpm
      gnumake
      pandoc
      texlive.combined.scheme-small
      yamllint
      # Databases
      pkgs.pgcli
      pkgs.mycli
      pkgs.litecli
      sqlite
      # cachix
      cachix
      nix-index
      nixos-rebuild
      # tools rewritten in rust
      hyperfine
      bandwhich
      # development
      tig
      sshuttle
      act
      nodejs
      elixir
    ];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      vimdiffAlias = true;
    };
    emacs.emacsPackage = pkgs.unstable.emacs29-pgtk.overrideAttrs (old: {
      patches = (old.patches or []) ++ [
        # Fix OS window role (needed for window managers like yabai)
        # (pkgs.fetchpatch {
        #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/fix-window-role.patch";
        #   sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE";
        # })
        # Use poll instead of select to get file descriptors
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
          sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY";
        })
        # Enable rounded window with no decoration
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
          sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4";
        })
        # Make Emacs aware of OS-level light/dark mode
        # (pkgs.fetchpatch {
        #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/system-appearance.patch";
        #   sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8";
        # })
      ];
    });
  };


  # nix settings
  nixpkgs.hostPlatform = "aarch64-darwin";
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.unstable.nix;
    linux-builder = {
      enable = false;
      ephemeral = true;
      maxJobs = 4;
      config = {
        virtualisation = {
          darwin-builder = {
            diskSize = 50 * 1024;
            memorySize = 6 * 1024;
          };
          cores = 4;
        };
      };
    };
    distributedBuilds = true;
    settings = {
      auto-optimise-store = false;
      sandbox = false;
      substituters = [
        "https://fbegyn-personal.cachix.org"
        "https://nix-community.cachix.org"
        "https://cuda-maintainers.cachix.org"
      ];
      trusted-public-keys = [
        "fbegyn-personal.cachix.org-1:0BEArpeI+ISsPainphPLHBozpP+zExYO6+43lLORDnI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
      trusted-users = [ "root" "francis" ];
      experimental-features = "nix-command flakes";
      system-features = [ "big-parallel" "benchmark" "nixos-test" "apple-virt" ];
    };
    extraOptions = ''
      extra-nix-path = nixpkgs=flake:nixpkgs
      bash-prompt-prefix = (nix:$name)\040
    '';
  };
}
