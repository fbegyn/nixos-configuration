{ config, pkgs, ... }:

{
  home.stateVersion = "25.11";

  # TODO: not darwin compatible
  # programs.ghostty = {
  #   enable = true;
  # };

  imports = [
    ./hm/base.nix
    ./hm/colors.nix
    ./hm/go.nix
    ./hm/python.nix
    ./hm/configurations/git.nix
    ./hm/configurations/bash.nix
    ./hm/configurations/fish.nix
    ./hm/configurations/fzf.nix
    ./hm/configurations/hledger.nix
    ./hm/configurations/emacs
    ./hm/configurations/nvim
    # ./hm/configurations/mpv TODO: not needed in base home-manager
    ./hm/configurations/direnv.nix
    # ./hm/configurations/zathura.nix TODO: I don't think I need this everywhere
    # ./hm/configurations/udiskie.nix TODO: not mac compatible
    # ./hm/configurations/alacritty # TODO: still needed?
    # ./hm/configurations/tmux TODO: still needed? Not used in a long time
    # ./hm/configurations/redshift.nix # TODO: not mac compatible
  ];

  xdg.configFile = {
    "qutebrowser/css/solarized-dark-all-sites.css".source =
      ./hm/configurations/qutebrowser/solarized-dark-all-sites.css;
    "home-manager/home.nix".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-configuration/users/francis/home.nix";
    "home-manager/flake.nix".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nixos-configuration/flake.nix";
  };

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

  home.enableNixpkgsReleaseCheck = false;
  programs.home-manager = {
    enable = true;
  };

  home.packages = with pkgs.unstable; [
    ripgrep
    # projecteur - TODO:not mac compatible
    fd
    envconsul
    cue
    age
    difftastic
    mergiraf
    pkgs.weechat
    age
    vdirsyncer
    khard
    inetutils
    pciutils
    usbutils
    # nodejs
    gcc
    (aspellWithDicts (dicts: with dicts; [en en-science nl]))
    pkgs.thunderbird
    # Browser
    pkgs.firefox
    # entertainement
    # playerctl TODO: not mac compatible
    pulsemixer
    # Utilities
    # lm_sensors TODO: not mac compatible
    gnumake
    tig
    sshuttle
    unzip
    pandoc
    pkgs.pgcli
    pkgs.mycli
    pkgs.litecli
    sqlite
    nix-index
    # tools rewritten in rust
    jujutsu
    curl
    wget
    envsubst
    keepassxc
    aerc
    rclone
    rsync
    yq
    jq
    tree-sitter
  ];
}
