{config, pkgs, ... }:
{
  # system packages to install
  environment.systemPackages = with pkgs.unstable; [
    tmux
    ldns
    sshpass
  ];

  system.stateVersion = 5;

  fonts = {
    packages = with pkgs; [
      nerdfonts
    ];
  };

  security.pam.enableSudoTouchIdAuth = true;

  users.users.francis.home = "/Users/francis";
  users.users.francis.shell = pkgs.fish;
  environment = {
    shells = with pkgs; [
      bash
      bashInteractive
      fish
    ];
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
   completion.enable = true;
   interactiveShellInit = let
     gitPrompt = builtins.fetchurl {
       url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh";
       sha256 = "0fllfidrc9nj2b9mllf190y3bca1pdm95vyzgsza1l3gl3s1ixvz";
     };
     gitCompletion = builtins.fetchurl {
       url = "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash";
       sha256 = "106wrn2wspci19a70006g5xsh679ap2973h2lmssf5xbl3r3lv7g";
     };
   in ''
     if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
       then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${config.home-manager.users.francis.programs.fish.package}/bin/fish $LOGIN_OPTION
     fi

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
    onActivation = {
      autoUpdate = false;
    };
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
      "nikitabobko/tap/aerospace"
    ];
    taps = [];
    brews = [
      "htop"
      "nmap"
      "gnupg"
      "yubikey-personalization"
      "ykman"
      "librsvg"
      "texlive"
      "pinentry-mac"
      "wget"
    ];
  };
  # launchd config
  # launchd.user.envVariables.PATH = config.environment.systemPath;
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
  home-manager.users.francis = {
    home.stateVersion = "24.05";
    imports = [
      ../../users/francis/hm/colors.nix
      ../../users/francis/hm/go.nix
      ../../users/francis/hm/configurations/josm.nix
      ../../users/francis/hm/configurations/fzf.nix
      ../../users/francis/hm/configurations/emacs
      ../../users/francis/hm/configurations/nvim
      ../../users/francis/hm/configurations/hledger.nix
      ../../users/francis/hm/configurations/direnv.nix
      ../../users/francis/hm/configurations/fish.nix
      ../../users/francis/hm/configurations/git.nix
    ];

    programs.home-manager.enable = true;
    programs.neovim = {
      enable = true;
      vimAlias = true;
      vimdiffAlias = true;
      defaultEditor = false;
    };

    home.sessionPath = [
      "$HOME/.go/bin"
      "$HOME/.local/bin"
      "$HOME/.cargo/bin"
    ];

    nixpkgs.config = {
      allowUnfree = true;
    };

    home.enableNixpkgsReleaseCheck = false;
    home.packages = with pkgs.unstable; [
      curl
      wget
      envsubst
      rclone
      rsync
      home-manager
      yq
      jq
      # Comms
      pkgs.openscad
      # SRE - deployment
      flyctl
      ansible
      # Utilities
      yt-dlp
      mpv
      open-in-mpv
      typst
      typst-lsp
      typst-fmt
      typst-live
      tree-sitter
      typescript-language-server
      wakeonlan
      ripgrep
      fd
      bat
      discord
      inetutils
      nfpm
      gnumake
      pandoc
      # texlive.combined.scheme-medium
      yamllint
      # Databases
      # pkgs.pgcli
      # pkgs.mycli
      # pkgs.litecli
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

    # emacs.emacsPackage = pkgs.unstable.emacs-macport;
    emacs.emacsPackage = pkgs.emacs29;
    # emacs.emacsPackage = pkgs.unstable.emacs30;
    # emacs.emacsPackage = pkgs.emacs29.overrideAttrs (old: {
    #   patches = (old.patches or []) ++ [
    #     # Fix OS window role (needed for window managers like yabai)
    #     (pkgs.fetchpatch {
    #       url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
    #       sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE";
    #     })
    #     # Enable rounded window with no decoration
    #     (pkgs.fetchpatch {
    #       url =
    #         "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
    #       sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4";
    #     })
    #     # Make Emacs aware of OS-level light/dark mode
    #     (pkgs.fetchpatch {
    #       url =
    #         "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
    #       sha256 = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw";
    #     })
    #     (pkgs.fetchpatch {
    #       url =
    #         "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
    #       sha256 = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c";
    #     })
    #   ];
    # });
  };


  # nix settings
  nixpkgs.hostPlatform = "aarch64-darwin";
  services.nix-daemon.enable = true;
  nix = {
    # package = pkgs.unstable.nix;
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
        "https://cache.lix.systems"
      ];
      trusted-public-keys = [
        "fbegyn-personal.cachix.org-1:0BEArpeI+ISsPainphPLHBozpP+zExYO6+43lLORDnI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
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
