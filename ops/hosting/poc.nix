{
  network = {
    description = "Cloud hosted servers";
  };

  "unifi-01" = { config, pkgs, lib, ... }: {
    imports = [
      ../../hosts/unifi-01/configuration.nix
    ];

    deployment.targetUser = "francis";
    deployment.targetHost = "65.108.59.13";
  };
}
