{ config, lib, pkgs, ... }:

with lib; {
  boot.cleanTmpDir = true;
  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "unlimited";
  }];

  nixpkgs.config = {
    allowUnfree = true;
  };

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      sandbox = true;
      trusted-users = [ "root" "francis" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

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
