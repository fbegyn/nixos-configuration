{ config, lib, pkgs, ... }:

with lib; {
  imports = [
    ./auto-system.nix
    ./network-tools.nix
    ./fwupd.nix
  ];

  options.francis = {
    gui.enable = mkEnableOption "Enable GUI programs";
  };

  config = {

    boot.cleanTmpDir = true;

    nix = {
      autoOptimiseStore = true;
      useSandbox = true;

      binaryCaches = [ "https://nix-community.cachix.org" ];
      binaryCachePublicKeys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];

      trustedUsers = [ "root" "francis" ];
    };

    nixpkgs.config = {
      allowUnfree = true;
      packageOverrides = import ../pkgs;
      overlays = [
      ];
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

    services.resolved = {
      enable = false;
      dnssec = "false";
      fallbackDns = [
        "1.1.1.1"
        "8.8.8.8"
        "1.0.0.1"
        "8.8.4.4"
        # "2606:4700:4700::1111"
        # "2001:4860:4860::8888"
        # "2606:4700:4700::1001"
        # "2001:4860:4860::8844"
      ];
    };
  };
}
