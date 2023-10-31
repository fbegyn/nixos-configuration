{ config, lib, pkgs, ... }:
{
  options.francis.emacs = {
    fullConfig = lib.mkOption {
      readOnly = true;
      default = builtins.readFile ./base-init.el + (
        lib.concatStringsSep "\n" config.francis.emacs.extraConfig
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
        config = config.francis.emacs.fullConfig;
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
          ${config.francis.emacs.package}/bin/emacsclient -t -a "" $@
        '';
        executable = true;
      };
      ".local/bin/ew" = {
        text = ''
          #!/bin/sh
          ${config.francis.emacs.package}/bin/emacsclient -a "" -nc $@
        '';
        executable = true;
      };
      ".emacs.d/init.el".text = config.francis.emacs.fullConfig;
      ".emacs.d/early-init.el".source = ./early-init.el;
    };

    xdg.configFile = {
      "emacs/init.el".text = config.francis.emacs.fullConfig;
      "emacs/early-init.el".source = ./early-init.el;
    };

    programs.emacs = {
      enable = true;
      package = config.francis.emacs.package;
    };
  };
}
