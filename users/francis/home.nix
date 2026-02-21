{ config, pkgs, lib, ... }:

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

  xdg.configFile = let
        symLink = config.lib.file.mkOutOfStoreSymlink;
        symLinkHome = path: symLink "${config.home.homeDirectory}/${path}";
        symLinkHM = path: symLink "${config.home.homeDirectory}/nixos-configuration/users/francis/hm/${path}";
  in {
    "home-manager/home.nix".source = symLinkHome "nixos-configuration/users/francis/home.nix";
    "home-manager/flake.nix".source = symLinkHome "nixos-configuration/flake.nix";
    "nvim/init.lua".source = symLinkHM "configurations/nvim/init.lua";
    "nvim/lua".source = symLinkHM "configurations/nvim/lua";
    "ghostty/config".source = symLinkHM "configurations/ghostty/config";
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
    fd
    envconsul
    cue
    vault-bin
    age
    difftastic
    mergiraf
    pkgs.weechat
    vdirsyncer
    khard
    inetutils
    gcc
    (aspellWithDicts (dicts: with dicts; [en en-science nl]))
    gnumake
    tig
    sshuttle
    unzip
    pandoc
    typst
    pkgs.pgcli
    pkgs.mycli
    pkgs.litecli
    sqlite
    nix-index
    jujutsu
    curl
    wget
    envsubst
    keepassxc
    aerc
    rclone
    rsync
    pkgs.firefox
    google-cloud-sdk
    yq
    jq
    tree-sitter
    # fonts
    pkgs.nerd-fonts.dejavu-sans-mono
    pkgs.nerd-fonts.fira-code
    pkgs.nerd-fonts.terminess-ttf
    pkgs.terminus_font_ttf
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    pciutils
    usbutils
    pkgs.thunderbird
    pulsemixer
  ] ++ lib.optionals pkgs.stdenv.isDarwin [

  ];
}
