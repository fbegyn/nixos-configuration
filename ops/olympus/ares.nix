{
  ares =
    { config, pkgs, ... }:
    {
      deployment = {
        targetHost = "10.3.10.10";
      };

      imports = [
        ../../hosts/ares/configuration.nix
      ];
    };
}
