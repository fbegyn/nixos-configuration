{config, pkgs, ... }:
{
  # system packages to install
  environment.systemPackages = with pkgs.unstable; [
    tmux
    ldns
    sshpass
  ];

  fonts = {
    packages = with pkgs; [
      nerdfonts
    ];
  };

  users.users.francis.home = "/Users/francis";
  environment = {
    shells = with pkgs; [
      bash
      bashInteractive
      fish
    ];
    loginShell = pkgs.bashInteractive;
    systemPath = [
      "/Users/francis/.local/bin"
      "/Users/francis/.go/bin"
      "/opt/homebrew/bin"
    ];
    variables = {
      SSH_AUTH_SOCK = "/Users/francis/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh";
    };
  };
  services.tailscale.enable = true;
  programs.bash = {
   enable = true;
   enableCompletion = true;
   interactiveShellInit = let
     gitPrompt = builtins.fetchurl {
       url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh";
       sha256 = "1bkdllwxfbcbflfi6w4p2ls8hvqpv2hwvqf5fw3w4zh89p2vg5ra";
     };
     gitCompletion = builtins.fetchurl {
       url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash";
       sha256 = "106wrn2wspci19a70006g5xsh679ap2973h2lmssf5xbl3r3lv7g";
     };
   in ''
     # load git-prompt script
     . ${gitPrompt}
     # load git completions
     . ${gitCompletion}

     PS1='\[\e[32m\]\u\[\e[0m\]@\[\e[38;5;126m\]\h\[\e[0m\] \[\e[38;5;40m\]\w\[\e[38;5;147m\]$(__git_ps1 " (%s)")\[\e[0m\]> '

     SSH_AUTH_SOCK=/Users/francis/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
   '';
  };
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
      "nmap"
      "gnupg"
      "yubikey-personalization"
      "ykman"
      "pinentry-mac"
      "wget"
    ];
  };
  # launchd config
  launchd.user.envVariables.PATH = config.environment.systemPath;
  # darwin - emacs
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
    # ../../common/network-tools.nix
  ];
  home-manager.users.francis = {
    home.stateVersion = "24.05";
    imports = [
      ../../users/francis/hm/base.nix
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/josm.nix
      ../../users/francis/hm/configurations/fzf.nix
      ../../users/francis/hm/configurations/emacs
      ../../users/francis/hm/configurations/mpv
      ../../users/francis/hm/configurations/hledger.nix
      ../../users/francis/hm/configurations/direnv.nix
      ../../users/francis/secrets/bash.nix
    ];

    home.enableNixpkgsReleaseCheck = false;

    home.packages = with pkgs.unstable; [
      home-manager
      # Comms
      openscad
      # SRE - deployment
      flyctl
      ansible
      # Utilities
      typst
      typst-lsp
      typst-fmt
      typst-live
      tree-sitter
      typescript-language-server
      kitty
      wakeonlan
      ripgrep
      fd
      bat
      discord
      inetutils
      nfpm
      gnumake
      pandoc
      texlive.combined.scheme-medium
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
      elixir_1_17
      elixir-ls
    ];

    emacs.emacsPackage = pkgs.unstable.emacs29-pgtk.overrideAttrs (old: {
      patches = (old.patches or []) ++ [
        # Fix OS window role (needed for window managers like yabai)
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
          sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE";
        })
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
        (pkgs.fetchpatch {
          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
          sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8";
        })
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
    gc = {
      automatic = true;
      interval = { Weekday = 0; Hour = 0; Minute = 0; };
      options = "--delete-older-than 30d";
    };
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
