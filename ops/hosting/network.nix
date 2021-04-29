{
  network = {
    description = "Cloud hosted servers";
  };

  "hosting-01" = { config, pkgs, lib, ... }: {
    imports = [
      ../../hosts/hosting-01/configuration.nix
    ];

    deployment.targetUser = "francis";
    deployment.targetHost = "100.93.146.4";
  };
}
