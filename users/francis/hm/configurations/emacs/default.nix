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
      default = if pkgs.stdenv.isDarwin
                then pkgs.unstable.emacs-macport
                else pkgs.unstable.emacs31-pgtk;
    };
    package = lib.mkOption {
      default = pkgs.emacsWithPackagesFromUsePackage {
        config = cfg.fullConfig;
        package = cfg.emacsPackage;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          epkgs.majutsu
          (epkgs.treesit-grammars.with-grammars (g: with g; [
            tree-sitter-go tree-sitter-gomod tree-sitter-python tree-sitter-bash
            tree-sitter-dockerfile tree-sitter-yaml tree-sitter-elixir tree-sitter-heex
            tree-sitter-json tree-sitter-toml
          ]))
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
      gopls                  # go-ts-mode LSP
      gotools                # goimports
      rust-analyzer
      pyright
      ansible-language-server # fb/ansible-mode LSP
      ansible-lint           # used by ansible-language-server
      yaml-language-server   # yaml-ts-mode LSP
      terraform-ls           # terraform-mode LSP
      dockerfile-language-server # dockerfile-ts-mode LSP
      ruff
      nil                    # nix-mode LSP
      elixir-ls              # elixir(-ts)-mode LSP
      deno                   # deno-ts-mode LSP (deno lsp)
      bash-language-server   # bash-ts-mode LSP
      shellcheck             # bash-language-server diagnostics
      shfmt                  # bash-language-server formatting
      dasel                  # pet: parse pyproject.toml / venv config
    ] ++ lib.optionals pkgs.stdenv.isDarwin [
      pkgs.unstable.coreutils-prefixed  # provides `gls` for emacs dired on macOS
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

    services.emacs = {
      enable = true;
      package = cfg.package;
    };
  };
}
