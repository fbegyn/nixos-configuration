{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    flox = {
      url = "github:flox/flox/v1.4.4";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ghostty = {
      url = "github:ghostty-org/ghostty/v1.3.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    website = {
      url = "github:fbegyn/website";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{
    self,
    disko,
    nixpkgs,
    nixpkgs-unstable,
    nixos-hardware,
    darwin,
    flox,
    mac-app-util,
    home-manager,
    agenix,
    llm-agents,
    ghostty,
    emacs-overlay,
    website,
    noctalia,
  }: let
    overlay = final: prev: {
      unstable = import nixpkgs-unstable {
        system = prev.pkgs.stdenv.hostPlatform.system;
        config.allowUnfree = true;
        overlays = [
          (import ./overlays/weechat.nix)
          (import ./overlays/browser-eid.nix)
          lix-overlay
        ];
      };
      fbegyn = {
        website = final.callPackage ./pkgs/fbegyn/website.nix {};
        f1multiviewer = final.callPackage ./pkgs/fbegyn/f1multiviewer.nix {};
        brother-hll2375dw-driver = final.callPackage ./pkgs/brother/drivers/hll2375dw-cups.nix {};
      };
    };
    lix-overlay = final: prev: {
      inherit (prev.lixPackageSets.stable)
      nixpkgs-review
      nix-eval-jobs
      nix-fast-build
      colmena;
    };

    baseOverlays = [
      overlay
      lix-overlay
      emacs-overlay.overlay
      llm-agents.overlays.shared-nixpkgs
    ];

    basePackages = {pkgs, ...}: {
      environment.systemPackages = [
        flox.packages.${pkgs.stdenv.hostPlatform.system}.default
        agenix.packages.${pkgs.stdenv.hostPlatform.system}.default
      ];
    };

    mkMachine = extraModules:
      nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ({config, pkgs, ...}: {
            nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.hostPlatform = "x86_64-linux";
            nixpkgs.overlays = [ (import ./overlays/weechat.nix) ]
              ++ baseOverlays
              ++ [ ghostty.overlays.default ];
          })
          basePackages
          disko.nixosModules.disko
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
          ./common
        ] ++ extraModules;
      };

    mkMac = extraModules:
      darwin.lib.darwinSystem {
        modules = [
          ({config, pkgs, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.hostPlatform = "aarch64-darwin";
            nixpkgs.overlays = baseOverlays;
          })
          basePackages
          agenix.darwinModules.age
          home-manager.darwinModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
        ] ++ extraModules;
      };

    mkHome = homeSystem: username: homeDir: extraModules:
      home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          system = homeSystem;
          config.allowUnfree = true;
          overlays = baseOverlays ++ [ ghostty.overlays.default ];
        };
        extraSpecialArgs = { inherit nixpkgs; };
        modules = [
          {
            home.username = username;
            home.homeDirectory = homeDir;
            programs.go.env.GOPATH = "${homeDir}/.go";
          }
          ./users/francis/home.nix
        ] ++ extraModules;
      };

    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
  in {
    overlays.default = overlay;

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = [
          agenix.packages.${system}.agenix
          pkgs.nixfmt-rfc-style
          pkgs.statix
          pkgs.deadnix
        ];
      };
    });

    formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);

    # build every Linux host so evaluation regressions are caught by `nix flake check`
    checks.x86_64-linux = nixpkgs.lib.mapAttrs'
      (name: cfg: nixpkgs.lib.nameValuePair "nixos-${name}" cfg.config.system.build.toplevel)
      self.nixosConfigurations;

    darwinConfigurations = {
      erebus = mkMac [
        ./hosts/erebus/configuration.nix
        # mac-app-util disabled on erebus: its cl-nix-lite/SBCL dependency
        # fails to build on macOS 27 / Darwin 27 (SBCL cannot mmap its
        # fixed-address heap at 0x300100000 on the new kernel). Re-enable
        # once nixpkgs ships a Darwin-27-compatible SBCL.
        # mac-app-util.darwinModules.default
        # and add to home-manager.users.francis.imports:
        #   mac-app-util.homeManagerModules.default
      ];
    };

    homeConfigurations = {
      "francis-linux" = mkHome "x86_64-linux" "francis" "/home/francis" [];
      "francis-mac" = mkHome "aarch64-darwin" "francis" "/Users/francis" [];
      "vib-mac" = mkHome "aarch64-darwin" "francis.begyn" "/Users/francis.begyn" [
        {
          targets.darwin.linkApps.enable = false;
          targets.darwin.copyApps.enable = false;
        }
      ];
    };

    nixosConfigurations = {
      bia = mkMachine [
        ./hosts/bia/configuration.nix
        nixos-hardware.nixosModules.common-cpu-amd
        noctalia.nixosModules.default
      ];
      ania = mkMachine [
        ./hosts/ania/configuration.nix
        nixos-hardware.nixosModules.common-pc-laptop
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
      geras = mkMachine [
        ./hosts/geras/configuration.nix
        nixos-hardware.nixosModules.common-pc-laptop
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
      eos = mkMachine [
        ./hosts/eos/configuration.nix
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
      zima432 = mkMachine [
        ./hosts/zima432/configuration.nix
      ];
      selene = mkMachine [
        ./hosts/selene/configuration.nix
      ];
      nix-builder-01 = mkMachine [
        ./hosts/nix-builder-01/configuration.nix
      ];
    };
    packages.x86_64-linux = {
      geras-format = (nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "format" ''
        ${self.nixosConfigurations.geras.config.system.build.diskoScript}
      '');
    };
  };
}
