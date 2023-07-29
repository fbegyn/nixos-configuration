{ config, lib, pkgs, ... }:

let
  e = pkgs.writeTextFile {
    name = "francis-emacs.desktop";
    destination = "/share/applications/francis-emacs.desktop";
    text = ''
[Desktop Entry]
Exec=${config.francis.emacs.package}/bin/emacsclient -nc
Icon=emacs
Name[en_US]=Emacs Client
Name=Emacs Client
StartupNotify=true
Terminal=false
Type=Application
      '';
    };
in
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
      readOnly = true;
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
      e
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
