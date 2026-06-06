{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    flox = {
      url = "github:flox/flox/v1.4.4";
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.95.2-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    ghostty = {
      url = "github:ghostty-org/ghostty/v1.2.3";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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
    mac-app-util.url = "github:hraban/mac-app-util";

    disko.url = "github:nix-community/disko";
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
    flake-utils-plus,
    flake-utils,
    home-manager,
    home-manager-unstable,
    lix-module,
    agenix,
    devshell,
    ghostty,
    emacs-overlay,
    website,
    noctalia,
  }: let
    overlay = final: prev: {
      unstable = import nixpkgs-unstable {
        inherit nixpkgs;
        system = prev.pkgs.stdenv.hostPlatform.system;
        config.allowUnfree = true;
        overlays = [
          (import ./overlays/weechat.nix)
          (import ./overlays/browser-eid.nix)
          lix-overlay
        ];
      };
      fbegyn = {
        inherit nixpkgs;
        website = nixpkgs.callPackage ./fbegyn/website.nix {};
        f1multiviewer = nixpkgs.callPackage ./fbegyn/f1multiviewer.nix {};
        brother-hll2375dw-driver = nixpkgs.callPackage ./brother/drivers/hll2375dw-cups.nix {};
      };
    };
    lix-overlay = final: prev: {
      inherit (prev.lixPackageSets.stable)
      nixpkgs-review
      nix-eval-jobs
      nix-fast-build
      colmena;
    };

    mkMachine = extraModules:
      nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ({config, pkgs, ...}: {
            nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.hostPlatform = "x86_64-linux";
            nixpkgs.overlays = [
              (import ./overlays/weechat.nix)
              overlay
              lix-overlay
              emacs-overlay.overlay
              ghostty.overlays.default
            ];
            environment.systemPackages = [
              flox.packages.${pkgs.stdenv.hostPlatform.system}.default
              agenix.packages.${pkgs.stdenv.hostPlatform.system}.default
            ];
          })
          # lix-module.nixosModules.default
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
            nixpkgs.overlays = [
              overlay
              lix-overlay
              emacs-overlay.overlay
            ];
            environment.systemPackages = [
              flox.packages.${pkgs.stdenv.hostPlatform.system}.default
              agenix.packages.${pkgs.stdenv.hostPlatform.system}.default
            ];
          })
          agenix.darwinModules.age
          # lix-module.nixosModules.default
          home-manager.darwinModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
        ] ++ extraModules;
      };

      mkHome = homeSystem: username: homeDir: extraModules:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."${homeSystem}";
          extraSpecialArgs = { inherit nixpkgs; };
          modules = [
            ({config, ...}: {
              nixpkgs.config.allowUnfree = true;
              nixpkgs.overlays = [
                overlay
                lix-overlay
                emacs-overlay.overlay
                ghostty.overlays.default
              ];
              home.username = "${username}";
              home.homeDirectory = "${homeDir}";
              programs.go.env.GOPATH = "${homeDir}/.go";
            })
            ./users/francis/home.nix
          ];
        };
  in {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages."x86_64-linux".mkShell {
      buildInputs = [
        agenix.packages.x86_64-linux.agenix
      ];
    };

    darwinConfigurations = {
      erebus = mkMac [
        ./hosts/erebus/configuration.nix
        mac-app-util.darwinModules.default
        ({pkgs, config, inputs, ...}: {
          environment.systemPackages = [
            flox.packages.${pkgs.stdenv.hostPlatform.system}.default
          ];
          home-manager.users.francis.imports = [
            mac-app-util.homeManagerModules.default
          ];
        })
      ];
    };

    homeConfigurations = {
      "francis-linux" = mkHome "x86_64-linux" "francis" "/home/francis" [];
      "francis-mac" = mkHome "aarch64-darwin" "francis" "/Users/francis" [];
      "vib-mac" = mkHome "aarch64-darwin" "francis.begyn" "/Users/francis.begyn" [];
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
  };
}
