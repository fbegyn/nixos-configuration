{
  ares =
    { ... }:
    {
      deployment.targetHost = "10.3.21.33";
      imports = [
        ../../hosts/ares/configuration.nix
      ];
    };
}
