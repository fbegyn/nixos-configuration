{ config, lib, pkgs, ... }:

let
  cfg = config.emacs;
in {
  options.emacs = {
    fullConfig = lib.mkOption {
      readOnly = true;
      default = builtins.readFile ./base-init.el + (
        lib.concatStringsSep "\n" cfg.extraConfig
        ) + ''
          (provide 'init)
          ;;; init.el ends here
        '';
    };
    extraConfig = lib.mkOption {
      default = [];
    };
    emacsPackage = lib.mkOption {
      default = pkgs.emacs-pgtk;
    };
    package = lib.mkOption {
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = cfg.fullConfig;
        package = cfg.emacsPackage;
        alwaysEnsure = true;
        # extraEmacsPackages = epkgs: [];
      };
    };
  };

  config = {
    home.packages = with pkgs.unstable; [
      ispell
      solargraph
      rust-analyzer
      lua53Packages.digestif
      rnix-lsp
      pyright
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
      ".emacs.d/early-init.el".source = ./early-init.el;
      ".emacs.d/init.el".text = cfg.fullConfig;
    };

    xdg.configFile = {
      "emacs/early-init.el".source = ./early-init.el;
      "emacs/init.el".text = cfg.fullConfig;
    };

    programs.emacs = {
      enable = true;
      package = cfg.package;
    };
  };
}
