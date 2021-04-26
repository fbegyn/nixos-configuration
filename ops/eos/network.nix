{
  network = {
    description = "Homelab server and infra";
  };

  "eos" = { config, pkgs, lib, ... }: {
    imports = [
      ../../hosts/eos/configuration.nix
    ];

    deployment.targetUser = "francis";
    deployment.targetHost = "10.5.1.10";
  };
}
