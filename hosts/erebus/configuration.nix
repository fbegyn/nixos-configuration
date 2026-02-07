{config, pkgs, lib, ... }:
let
  sshAuthSock = "/Users/francis/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh";
in {
  # system packages to install
  environment.systemPackages = with pkgs.unstable; [
    tmux
    ldns
    sshpass
    openssh
    nixos-generators
    mosh
  ];

  system.stateVersion = 5;

  fonts = {
    packages = with pkgs; [
      nerd-fonts.dejavu-sans-mono
      nerd-fonts.fira-code
      nerd-fonts.terminess-ttf
      terminus_font_ttf
    ];
  };

  security.pam.services.sudo_local.touchIdAuth = true;

  system.primaryUser = "francis";
  users.users.francis = {
    uid = 501;
    home = "/Users/francis";
    shell = pkgs.bashInteractive;
  };
  users.knownUsers = [ "francis" ];
  environment = {
    shells = with pkgs; [
      bashInteractive
      fish
    ];
    systemPath = [
      "/Users/francis/.local/bin"
      "/Users/francis/.go/bin"
      "/opt/homebrew/bin"
    ];
    variables = {
      SSH_AUTH_SOCK = "${sshAuthSock}";
      PAGER = "less";
      EDITOR = "nvim";
    };
  };
  services.tailscale.enable = true;
  programs.bash = {
    enable = true;
    completion.enable = true;
    interactiveShellInit = ''
     SSH_AUTH_SOCK=${sshAuthSock}
   '';
  };
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
     set SSH_AUTH_SOCK ${sshAuthSock}
   '';
  };

  # darwin specific modules
  # homebrew
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      cleanup = "uninstall";
      upgrade = true;
    };
    global = {
      brewfile = true;
    };
    casks = [
      "telegram"
      "google-chrome"
      "visual-studio-code"
      "orbstack"
      "freecad"
      "libreoffice"
      "tunnelblick"
      "secretive"
      "josm"
      "discord"
      "finch"
      "nikitabobko/tap/aerospace"
      "dagger/tap/container-use"
      "claude-code"
      "meld"
    ];
    taps = [
      "nikitabobko/tap"
      "felixkratz/formulae"
      "dagger/tap"
    ];
    brews = [
      "mr"
      "podman"
      "ffmpeg"
      "yt-dlp"
      "htop"
      "nmap"
      "gnupg"
      # "yubikey-personalization"
      "ykman"
      "librsvg"
      # "texlive"
      "pinentry-mac"
      "wget"
      "icann-rdap"
      "git-crypt"
      "borders"
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

  imports = [
    ../../users/francis/variables.nix
  ];

  # home-manager settings (darwin)
  home-manager.users.francis = {
    imports = [
      ../../users/francis/home.nix
    ];

    programs.go.env.GOPATH = "/Users/francis/.go";

    home.packages = with pkgs.unstable; [
      home-manager
    ];

    # emacs.emacsPackage = pkgs.unstable.emacs-macport;
    emacs.emacsPackage = pkgs.emacs30;
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
  # services.nix-daemon.enable = true;
  nix = {
    # useDaemon = true;
    # package = pkgs.nixVersions.nix_2_29;
    package = pkgs.unstable.lixPackageSets.latest.lix;
    distributedBuilds = true;
    gc = {
      automatic = true;
      interval = { Weekday = 0; Hour = 0; Minute = 0; };
      options = "--delete-older-than 30d";
    };
    optimise.automatic = true;
    buildMachines = [
    ];
    settings = {
      sandbox = false;
      substituters = [
        "https://fbegyn-personal.cachix.org"
        "https://nix-community.cachix.org"
        "https://cuda-maintainers.cachix.org"
        "https://cache.lix.systems"
        "https://cache.flox.dev"
      ];
      trusted-public-keys = [
        "fbegyn-personal.cachix.org-1:0BEArpeI+ISsPainphPLHBozpP+zExYO6+43lLORDnI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
      ];
      trusted-users = [ "root" "francis" "@admin" ];
      experimental-features = "nix-command flakes";
      system-features = [ "big-parallel" "benchmark" "nixos-test" "apple-virt" ];
    };
    extraOptions = ''
      extra-nix-path = nixpkgs=flake:nixpkgs
      bash-prompt-prefix = (nix:$name)\040
    '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin x86_64-linux aarch64-linux
    '';
  };
}
