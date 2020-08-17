{
  ares =
    { ... }:
    {
      deployment = {
        targetHost = "10.3.21.33";
        targetUser = "francis";
      };
      imports = [
        ../../hosts/ares/configuration.nix
      ];
    };
}
