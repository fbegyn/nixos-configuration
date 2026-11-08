{ config, pkgs, ... }:

{
  users.groups.thecy = { };
  systemd.services.thecy-homedir-setup = {
    description = "Creates homedirs for /srv/thecy services";
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = with pkgs; ''
      ${coreutils}/bin/mkdir -p /srv/thecy
      ${coreutils}/bin/chown root:thecy /srv/thecy
      ${coreutils}/bin/chmod 775 /srv/thecy
      ${coreutils}/bin/mkdir -p /srv/thecy/run
      ${coreutils}/bin/chown root:thecy /srv/thecy/run
      ${coreutils}/bin/chmod 770 /srv/thecy/run
    '';
  };
}
