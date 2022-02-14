# A collection of "uncontroversial" configurations for selected packages.

{ pkgs, lib, config, ... }:

{
  programs.emacs.init.usePackage = {
    all-the-icons = { extraPackages = [ pkgs.emacs-all-the-icons-fonts ]; };

    deadgrep = {
      config = ''
        (setq deadgrep-executable "${pkgs.unstable.ripgrep}/bin/rg")
      '';
    };

    dhall-mode = { mode = [ ''"\\.dhall\\'"'' ]; };

    dockerfile-mode = { mode = [ ''"Dockerfile\\'"'' ]; };

    emacsql-sqlite3 = {
      enable =
        lib.mkDefault config.programs.emacs.init.usePackage.org-roam.enable;
      defer = lib.mkDefault true;
      config = ''
        (setq emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3")
      '';
    };

    ggtags = {
      config = ''
        (setq ggtags-executable-directory "${pkgs.unstable.global}/bin")
      '';
    };

    markdown-mode = {
      mode = [ ''"\\.mdwn\\'"'' ''"\\.markdown\\'"'' ''"\\.md\\'"'' ];
    };

    nix-mode = { mode = [ ''"\\.nix\\'"'' ]; };

    org-roam = {
      config = ''
        (setq org-roam-graph-executable "${pkgs.unstable.graphviz}/bin/dot")
      '';
    };

    pandoc-mode = {
      config = ''
        (setq pandoc-binary "${pkgs.unstable.pandoc}/bin/pandoc")
      '';
    };

    ripgrep = {
      config = ''
        (setq ripgrep-executable "${pkgs.unstable.ripgrep}/bin/rg")
      '';
    };

    rust-mode = { mode = [ ''"\\.rs\\'"'' ]; };

    terraform-mode = { mode = [ ''"\\.tf\\'"'' ]; };

    yaml-mode = { mode = [ ''"\\.\\(e?ya?\\|ra\\)ml\\'"'' ]; };
  };
}
