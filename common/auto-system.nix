{ config, lib, pkgs, ... }:

let
  cfg = config.francis;
in
with lib; {
  options.francis = {
    upgrade = {
      enable = mkEnableOption "Enable automatic system updates";
      dates = mkOption {
        type = types.str;
        default = "23:10";
      };
    };
    gc = {
      enable = mkEnableOption "Enable automatic GC"; 
      dates = mkOption {
        type = types.str;
        default = "12:00";
      };
    };
  };

  config = {
    nix.gc = mkIf cfg.gc.enable {
      automatic = true;
      dates = cfg.gc.dates;
    };

    system.autoUpgrade = mkIf cfg.upgrade.enable {
      enable = true;
      allowReboot = false;
      dates = cfg.upgrade.dates;
    };
  };
}
