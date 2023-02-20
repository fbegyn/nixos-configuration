{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    utils.url = "github:numtide/flake-utils";
    utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
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
      url = "github:nix-community/emacs-overlay/2b807c388a5600d9f4e98f743306834de5dd03a5";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "utils";
      };
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    website = {
      url = "github:fbegyn/website";
      inputs = {
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };
  };

  outputs = inputs@{
    self,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    nixos-hardware,
    utils-plus,
    utils,
    home-manager,
    agenix,
    devshell,
    emacs-overlay,
    deploy-rs,
    website,
    nixos-mailserver
  }: let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";

    overlay = final: prev: {
      unstable = import nixpkgs-unstable {
        system = "x86_64-linux";
        inherit nixpkgs;
        config.allowUnfree = true;
      };
      fbegyn = {
        system = "x86_64-linux";
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
      eos = mkMachine [
        ./hosts/eos/configuration.nix
      ];
      ania = mkMachine [
        ./hosts/ania/configuration.nix
        nixos-hardware.nixosModules.common-pc-laptop
        nixos-hardware.nixosModules.common-pc-ssd
        nixos-hardware.nixosModules.common-cpu-intel
      ];
      hosting-01 = mkMachine [
        ./hosts/hosting-01/configuration.nix
        website.nixosModules.x86_64-linux.website
      ];
      mail-01 = mkMachine [
        ./hosts/mail-01/configuration.nix
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
