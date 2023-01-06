{ pkgs, lib, ... }:

{
  services.postgresql = {
    enable = true;
    ensureUsers = [
      {
        name = "francis";
        ensurePermissions = {
          "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  environment.systemPackages = with pkgs; [
    pgcli
  ];
}
