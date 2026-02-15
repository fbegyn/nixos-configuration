{ config, lib, pkgs, ... }:

let
  cfg = config.emacs;
in {
  options.emacs = {
    fullConfig = lib.mkOption {
      readOnly = true;
      default = builtins.readFile ./base-init.el;
    };
    emacsPackage = lib.mkOption {
      default = pkgs.unstable.emacs30-gtk3;
    };
    package = lib.mkOption {
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = cfg.fullConfig;
        package = cfg.emacsPackage;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          epkgs.majutsu
        ];
        override = epkgs: epkgs // {
          majutsu = epkgs.trivialBuild {
            pname = "majutsu";
            version = "0.6.0";
            src = pkgs.fetchFromGitHub {
              owner = "0WD0";
              repo = "majutsu";
              rev = "v0.6.0";
              hash = if pkgs.stdenv.hostPlatform.system == "aarch64-darwin"
              then "sha256-pdumBo9AwfpIZPolcFPGCIzTlNdH1mkagl6CcMcHBK0="
              else "sha256-pdumBo9AwfpIZPolcFPGCIzTlNdH1mkagl6CcMcHBK0=";
            };
            packageRequires = with epkgs; [ transient with-editor magit ];
          };
        };
      };
    };
  };

  config = let
    symLink = config.lib.file.mkOutOfStoreSymlink;
    emacsSymLink = param: symLink "${config.home.homeDirectory}/nixos-configuration/users/francis/hm/configurations/emacs/${param}";
  in {
    home.packages = with pkgs.unstable; [
      ispell
      solargraph
      rust-analyzer
      pyright
      nodePackages.bash-language-server
      # ansible-language-server
      ruff
      # nil
    ];

    home.file = {
      ".local/bin/e" = {
        text = ''
          #!/bin/sh
          ${cfg.package}/bin/emacsclient -t -a "" $@
        '';
        executable = true;
      };
      ".local/bin/ew" = {
        text = ''
          #!/bin/sh
          ${cfg.package}/bin/emacsclient -a "" -nc $@
        '';
        executable = true;
      };
      ".emacs.d/early-init.el".source = emacsSymLink "early-init.el";
      ".emacs.d/init.el".source = emacsSymLink "base-init.el";
    };

    xdg.configFile = {
      "emacs/early-init.el".source = emacsSymLink "early-init.el";
      "emacs/init.el".source = emacsSymLink "base-init.el";
    };

    programs.emacs = {
      enable = true;
      package = cfg.package;
    };
  };
}
