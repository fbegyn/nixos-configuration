{ pkgs, ... }:
{
  # system packages to install
  environment.systemPackages = with pkgs.unstable; [
    git
    vim
    alacritty
    vagrant
    tmux
    nfpm
    ripgrep
    ldns
    fish
    bat
    go_1_20
    (pkgs.emacsWithPackagesFromUsePackage {
      config = builtins.readFile ../../users/francis/hm/configurations/emacs/base-init.el;
      package = pkgs.emacs;
      alwaysEnsure = true;
    })
    curl
    wget
    starship
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
  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    global.brewfile = true;
    casks = [
      "docker"
      "google-chrome"
      "visual-studio-code"
      "discord"
      "libreoffice"
    ];
    taps = [];
    brews = [
      "htop"
    ];
  };

  # home-manager settings (darwin)
  # home-manager.users.francis = {
  #   home.stateVersion = "23.05";
  #   imports = [
  #   ];
  # };

  # nix settings
  nixpkgs.hostPlatform = "aarch64-darwin";
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.unstable.nix;
    settings = {
      auto-optimise-store = true;
      sandbox = true;
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
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      extra-nix-path = nixpkgs=flake:nixpkgs
      bash-prompt-prefix = (nix:$name)\040
    '';
  };
}
