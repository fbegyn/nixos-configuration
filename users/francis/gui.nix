{ config, lib, pkgs, ... }:

with lib;

let
  cfg = options.francis.gui;
in {
  options.francis.gui = {
    enable =
      mkEnableOption "Enable the gui applications of Francis home-manager";
    comms = mkOption {
      default = false;
      type = types.bool;
      description = "Whether to enable comm programs";
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.francis = {
      home.packages = with pkgs; [
      ] ++ comm;
    };
  };
}
