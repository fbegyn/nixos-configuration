{
  description = "A very basic flake";

  inputs = {
    nixos.url = "nixpkgs/nixos-22.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    utils = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs = {
        flake-utils.follows = "flake-utils";
      };
    };
    digga= {
      url = "github:divnix/digga";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
  };

  outputs = inputs@{
    self,
      nixos,
      nixpkgs,
      unstable,
      nur,
      nixos-hardware,
      flake-utils,
      utils,
      digga,
      home-manager,
      agenix,
      devshell,
      emacs-overlay,
      nixos-mailserver
  }: digga.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      channels = {
        nixos = {
          imports = [ (digga.lib.importOverlays ./overlays) ];
          overlays = [
          ];
        };
        nixpkgs = {
          imports = [ (digga.lib.importOverlays ./overlays) ];
          overlays = [
          ];
        };
        unstable = {};
      };

      sharedOverlays = [
        nur.overlay
        emacs-overlay.overlay
      ];

      nixos = {
        hostDefaults = {
          channelName = "nixos";
          modules = [
            { nix.generateRegistryFromInputs = true; }
            agenix.nixosModules.age
            home-manager.nixosModule
            nixos-mailserver.nixosModule
          ];
        };
        hosts = {
          horme.modules = [ ./hosts/horme/configuration.nix ];
        };
      };

      home = {
        users = digga.lib.rakeLeaves ./users/francis;
      };
  };
}
