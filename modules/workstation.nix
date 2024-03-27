{ config, lib, pkgs, ...}:

with lib;
let
  cfg = config.fbegyn.workstation;
in {
  options.fbegyn.workstation = {
    enable = mkEnableOption "Load all nixos config related to a workstation";
  };

  config = mkIf cfg.enable {
    imports = [
      ../roles/workstation
    ];
  };
}
