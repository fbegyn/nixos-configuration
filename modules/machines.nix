{ config, lib, pkgs, ...}:

with lib;
let
  cfg = config.fbegyn.machines;
in {
  options.fbegyn.machines = {
    roles = mkOption {
      type = types.str;
      default = "workstation";
      example = "server,bastion";
      description = "The roles this machine should take up";
    };
  };

  config = {
  };
}
