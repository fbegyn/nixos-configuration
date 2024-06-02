{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/87bfbcd248f3f17040db47b2cc8542033b12d051";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-23.11";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    vscode-server.url = "github:nix-community/nixos-vscode-server";
    website = {
      url = "github:fbegyn/website";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
  };

  outputs = inputs@{
    self,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    nixos-hardware,
    darwin,
    mac-app-util,
    flake-utils-plus,
    flake-utils,
    home-manager,
    home-manager-unstable,
    agenix,
    devshell,
    emacs-overlay,
    vscode-server,
    website,
    nixos-mailserver
  }: let
    overlay = final: prev: {
      unstable = import nixpkgs-unstable {
        system = prev.system;
        inherit nixpkgs;
        config.allowUnfree = true;
        overlays = [
          (import ./overlays/weechat.nix)
          (import ./overlays/browser-eid.nix)
        ];
      };
      fbegyn = {
        system = prev.system;
        website = nixpkgs.callPackage ./fbegyn/website.nix {};
        f1multiviewer = nixpkgs.callPackage ./fbegyn/f1multiviewer.nix {};
        brother-hll2375dw-driver = nixpkgs.callPackage ./brother/drivers/hll2375dw-cups.nix {};
      };
    };

    mkMachine = extraModules:
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              (import ./overlays/weechat.nix)
              overlay
              emacs-overlay.overlay
            ];
          })
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
          ./common
        ] ++ extraModules;
      };

    mkMac = extraModules:
      darwin.lib.darwinSystem rec {
        system = "aarch64-darwin";
        modules = [
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
              emacs-overlay.overlay
            ];
          })
          agenix.darwinModules.age
          home-manager-unstable.darwinModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
        ] ++ extraModules;
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
	        home-manager.users.francis.imports = [
	          mac-app-util.homeManagerModules.default
	        ];
	      })
      ];
    };

    nixosConfigurations = {
      horme = mkMachine [
        ./hosts/horme/configuration.nix
        nixos-hardware.nixosModules.common-pc-laptop
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
        nixos-hardware.nixosModules.lenovo-thinkpad
        nixos-hardware.nixosModules.lenovo-thinkpad-x1
      ];
      bia = mkMachine [
        ./hosts/bia/configuration.nix
        nixos-hardware.nixosModules.common-cpu-amd
        vscode-server.nixosModules.default
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
        vscode-server.nixosModules.default
      ];
      hosting-01 = mkMachine [
        ./hosts/hosting-01/configuration.nix
        website.nixosModules.x86_64-linux.website
      ];
      router-01 = mkMachine [
        ./hosts/router-01/configuration.nix
      ];
      zima432 = mkMachine [
        ./hosts/zima432/configuration.nix
      ];
      mail-01 = mkMachine [
        ./hosts/mail-01/configuration.nix
        nixos-mailserver.nixosModules.mailserver
      ];
      lan-app-01 = mkMachine [
        ./hosts/lan-party/app-01/configuration.nix
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
    };
  };
}
