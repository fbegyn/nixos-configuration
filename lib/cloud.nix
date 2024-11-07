{nixpkgs, ... }:

{
    mkCloudBox = hostname: {timeZone ? "Europe/Brussels", extraModules}:
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({config, ...}: {
            # set the hostname for the cloud box
            networking.hostname = "${hostname}";
            time.timeZone = "${timeZone}";

            # enable firewall, we're online on the scary internet after all
            networking.firewall.enable = true;

            # Enable node exporter on all cloud servers
            services.prometheus.exporters.node.enable = true;
            services.prometheus.exporters.node.enabledCollectors = [ "systemd" ];

            # default nix settings
            nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
            nixpkgs.config.allowUnfree = true;
          })
        ] ++ extraModules;
      };
}
