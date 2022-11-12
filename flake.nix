{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    utils = {
      url = "github:gytis-ivaskevicius/flake-utils-plus/v1.3.1";
      inputs = {
        flake-utils.follows = "flake-utils";
      };
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
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
  };

  outputs = inputs@{
    self,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    nixos-hardware,
    utils,
    flake-utils,
    home-manager,
    agenix,
    devshell,
    emacs-overlay,
    deploy-rs,
    nixos-mailserver
  }: utils.lib.mkFlake {
    inherit self inputs;

    channelsConfig = {
      allowUnfree = true;
    };

    sharedOverlays = let
      system = "x86_64-linux";
      overlay = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      };
    in [
      overlay
      nur.overlay
      emacs-overlay.overlay
    ];

    channels ={
      nixpkgs = {
        input = nixpkgs;
        overlaysBuilder = _: [
          devshell.overlay
        ];
      };
    };

    hostDefaults = {
      channelName = "nixpkgs";
      modules = [
        { nix.generateRegistryFromInputs = true; }
        agenix.nixosModules.age
        home-manager.nixosModules.home-manager ({config, ...}: {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        })
      ];
    };

    hosts = {
      horme = {
        modules = [
          ./hosts/horme/configuration.nix
          nixos-hardware.nixosModules.common-pc-laptop
          nixos-hardware.nixosModules.common-pc-ssd
          nixos-hardware.nixosModules.common-cpu-intel
          nixos-hardware.nixosModules.lenovo-thinkpad
          nixos-hardware.nixosModules.lenovo-thinkpad-x1
        ];
      };
      bia = {
        modules = [
          ./hosts/bia/configuration.nix
          nixos-hardware.nixosModules.common-cpu-amd
        ];
      };
      ania = {
        modules = [
          ./hosts/ania/configuration.nix
          nixos-hardware.nixosModules.common-pc-laptop
          nixos-hardware.nixosModules.common-pc-ssd
          nixos-hardware.nixosModules.common-cpu-intel
        ];
      };
      eos = {
        modules = [
          ./hosts/eos/configuration.nix
          nixpkgs.nixosModules.notDetected
        ];
      };
      mail-01 = {
        modules = [
          ./hosts/mail-01/configuration.nix
        ];
      };
      unifi-01 = {
        modules = [
          ./hosts/unifi-01/configuration.nix
        ];
      };
      hosting-01 = {
        modules = [
          ./hosts/hosting-01/configuration.nix
        ];
      };
    };

     # This is highly advised, and will prevent many possible mistakes
    checks = builtins.mapAttrs
      (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
  };
}
