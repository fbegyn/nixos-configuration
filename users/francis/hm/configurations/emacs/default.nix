{ config, lib, pkgs, ... }:

let
  cfg = config.francis.emacs;
in {
  options.francis.emacs = {
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
    package = lib.mkOption {
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = cfg.fullConfig;
        package = pkgs.emacs-pgtk;
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
    };

    xdg.configFile = {
      "emacs/init.el".text = cfg.fullConfig;
      "emacs/early-init.el".source = ./early-init.el;
    };

    programs.emacs = {
      enable = true;
      package = cfg.package;
    };
  };
}
