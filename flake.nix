{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };
    emacs-overlay = {
      url =
        "github:nix-community/emacs-overlay/dbb2bb4f40a27e2bba0bee5fe6f97e832a4583c6";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    vscode-server.url = "github:nix-community/nixos-vscode-server";
    website = {
      url = "github:fbegyn/website";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };

    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };

  outputs = inputs@{
    self,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    nixos-hardware,
    darwin,
    nixpkgs-darwin,
    flake-utils-plus,
    flake-utils,
    home-manager,
    agenix,
    devshell,
    emacs-overlay,
    deploy-rs,
    vscode-server,
    website,
    nixos-mailserver
  }: let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";

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
              nur.overlay
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
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({config, ...}: {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [
              (import ./overlays/weechat.nix)
              overlay
              nur.overlay
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
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = [
        deploy-rs.packages.x86_64-linux.deploy-rs
        agenix.packages.x86_64-linux.agenix
      ];
    };

    darwinConfigurations = {
      erebus = darwin.lib.darwinSystem {
        modules = [
          ./hosts/erebus/configuration.nix
        ];
      };
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
    };

    deploy.nodes.eos = {
      hostname = "eos";
      sshUser = "francis";
      profiles.system = {
        user = "root";
        path = deploy-rs.x86_64-linux.activate.nixos
          self.nixosConfigurations.eos;
      };
    };
    deploy.nodes.hosting-01 = {
      hostname = "hosting-01";
      sshUser = "francis";
      profiles.system = {
        user = "root";
        path = deploy-rs.x86_64-linux.activate.nixos
          self.nixosConfigurations.hosting-01;
      };
    };
    deploy.nodes.router-01 = {
      hostname = "router-01";
      sshUser = "francis";
      profiles.system = {
        user = "root";
        path = deploy-rs.x86_64-linux.activate.nixos
          self.nixosConfigurations.router-01;
      };
    };
    deploy.nodes.zima432 = {
      hostname = "zima432";
      sshUser = "francis";
      profiles.system = {
        user = "root";
        path = deploy-rs.x86_64-linux.activate.nixos
          self.nixosConfigurations.zima432;
      };
    };
    deploy.nodes.mail-01 = {
      hostname = "mail-01";
      sshUser = "francis";
      profiles.system = {
        user = "root";
        path = deploy-rs.x86_64-linux.activate.nixos
          self.nixosConfigurations.mail-01;
      };
    };
    # This is highly advised, and will prevent many possible mistakes
    checks = builtins.mapAttrs
      (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
  };
}
