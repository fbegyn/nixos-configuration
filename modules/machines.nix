{ config, lib, pkgs, ...}:

with lib;
let
  cfg = config.fbegyn.machines;
in {
  options.fbegyn.machines = {
    roles = mkOption {
      type = types.commas;
      default = "workstation";
      example = "server,bastion";
      description = "The roles this machine should take up";
    };
  };

  config = let
    work = test;
  in {
    environment.systemPackage = mkIf (builtins.elem "workstation" cfg.roles) [
      pkgs.hello
    ];

    fbegyn.workstation.enable = mkIf (builtins.elem "workstation" cfg.roles) true;
    fbegyn.webhost.enable = mkIf (builtins.elem "host" cfg.roles) true;
    fbegyn.bastion.enable = mkIf (builtins.elem "bastion" cfg.roles) true;
  };
}
