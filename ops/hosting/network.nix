{
  network = {
    description = "Cloud hosted servers";
  };

  "hosting-01" = { config, pkgs, lib, ... }: {
    imports = [
      ../../hosts/hosting-01/configuration.nix
    ];

    deployment.targetUser = "francis";
    deployment.targetHost = "135.181.105.123";
  };
}
