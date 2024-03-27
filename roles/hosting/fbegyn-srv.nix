{ config, pkgs, ... }:

{
  users.groups.fbegyn = { };
  systemd.services.fbegyn-homedir-setup = {
    description = "Creates homedirs for /srv/fbegyn services";
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = with pkgs; ''
      ${coreutils}/bin/mkdir -p /srv/fbegyn
      ${coreutils}/bin/chown root:fbegyn /srv/fbegyn
      ${coreutils}/bin/chmod 775 /srv/fbegyn
      ${coreutils}/bin/mkdir -p /srv/fbegyn/run
      ${coreutils}/bin/chown root:fbegyn /srv/fbegyn/run
      ${coreutils}/bin/chmod 770 /srv/fbegyn/run
    '';
  };
}
