{ nixpkgs, ... }:

{
  mkCloudBox = hostname: {
    system ? "x86_64-linux",
    timeZone ? "Europe/Brussels",
    extraModules,
    tailscale ? {
      enable = true;
      routingMode = "client";
      extraUpFlags = [
        "--advertise-tags=tag:prod,tag:hetzner,tag:cloud"
      ];
    }
  }: nixpkgs.lib.nixosSystem rec {
    inherit system;
    modules = [
      ({config, ...}: {
        # configure boot through systemd-boot
        boot.loader.grub.enable = false;
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

        # set the hostname for the cloud box
        networking.hostname = "${hostname}";
        networking.useDHCP = false;
        time.timeZone = "${timeZone}";

        # enable tailscale by default in client mode
        service.tailscale = {
          enable = tailscale.enable;
          openFirewall = false;
          useRoutingFeatures = "${tailscale.routingMode}";
          extraUpFlags = tailscale.extraUpFlags;
        };

        # enable firewall, we're online on the scary internet after all
        networking.firewall.enable = true;

        # we want ssh
        services.openssh = {
          enable = true;
          openFirewall = false;
        };

        # Enable node exporter on all cloud servers
        services.prometheus.exporters.node.enable = true;
        services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];

        # default nix settings
        nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
        nixpkgs.config.allowUnfree = true;

        home-manager.users.francis.home.stateVersion = "24.05";
        system.stateVersion = "24.05"; # Did you read the comment?
      })
    ] ++ extraModules;
  };
}
