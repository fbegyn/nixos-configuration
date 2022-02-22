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
    authentication = ''
      host all all 0.0.0.0/0 md5
    '';
  };

  environment.systemPackages = with pkgs; [
    pgcli
  ];
}
