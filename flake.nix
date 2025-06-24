{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    flox = {
      url = "github:flox/flox/v1.4.4";
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.93.0.tar.gz";
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
      url = "github:ghostty-org/ghostty";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-25.05";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    vscode-server.url = "github:nix-community/nixos-vscode-server";
    website = {
      url = "github:fbegyn/website";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
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
    nixos-generators,
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
          ({config, pkgs, ...}: {
            nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              (import ./overlays/weechat.nix)
              overlay
              emacs-overlay.overlay
              ghostty.overlays.default
            ];
            environment.systemPackages = [
              flox.packages.${pkgs.system}.default
              agenix.packages.${pkgs.system}.default
            ];
          })
          lix-module.nixosModules.default
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
          ./common
        ] ++ extraModules;
      };

    mkProxmoxLXC = extraModules:
      nixos-generators.nixosGenerate rec {
        system = "x86_64-linux";
        format = "proxmox-lxc";
        modules = [
          ({config, pkgs, ...}: {
            nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
            ];
            proxmoxLXC = {
              enable = true;
              privileged = true;
              manageNetwork = false;
              manageHostName = true;
            };
          })
          lix-module.nixosModules.default
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
          ({config, pkgs, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
              emacs-overlay.overlay
            ];
            environment.systemPackages = [
              flox.packages.${pkgs.system}.default
              agenix.packages.${pkgs.system}.default
            ];
          })
          agenix.darwinModules.age
          lix-module.nixosModules.default
          home-manager.darwinModules.home-manager ({config, ...}: {
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
            environment.systemPackages = [
              flox.packages.${pkgs.system}.default
            ];
	        home-manager.users.francis.imports = [
	          mac-app-util.homeManagerModules.default
	        ];
	      })
      ];
    };

    homeConfigurations = {
      "fbegyn" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = { inherit nixpkgs; };
        modules = [
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
              emacs-overlay.overlay
              ghostty.overlays.default
            ];
            home.username = "fbegyn";
            home.homeDirectory = "/home/fbegyn";
          })
          ./users/francis/home.nix
        ];
      };
    };

    nixosConfigurations = let
      cloud = import ./lib/cloud.nix {nixpkgs = nixpkgs;};
    in {
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
      hosting-01 = cloud.mkCloudGrub "hosting-01" {
        extraModules = [
          ./hosts/hosting-01/configuration.nix
          website.nixosModules.x86_64-linux.website
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
            ];
          })
        ];
      };
      hosting-02 = cloud.mkCloudGrub "hosting-02" {
        extraModules = [
          disko.nixosModules.disko
          agenix.nixosModules.age
          ./lib/hosting.nix
          ./hosts/hosting-02/disko.nix
          website.nixosModules.x86_64-linux.website
        ];
      };
      router-01 = mkMachine [
        ./hosts/router-01/configuration.nix
      ];
      zima432 = mkMachine [
        ./hosts/zima432/configuration.nix
      ];
      mail-01 = cloud.mkCloudGrub "mail-01" {
        extraModules = [
          ./hosts/mail-01/configuration.nix
          nixos-mailserver.nixosModules.mailserver
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          })
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              overlay
            ];
          })
        ];
      };
      lan-app-01 = mkMachine [
        ./hosts/lan-party/app-01/configuration.nix
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
      selene = mkMachine [
        ./hosts/selene/configuration.nix
      ];
      nix-builder-01 = mkMachine [
        ./hosts/nix-builder-01/configuration.nix
      ];
      lxc-template = mkProxmoxLXC [
        ./hosts/proxmox-ct-template/configuration.nix
      ];
    };
  };
}
