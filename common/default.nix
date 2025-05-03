{ config, lib, pkgs, ... }:

{
  boot.tmp.cleanOnBoot = true;
  boot.tmp.useTmpfs = true;
  systemd.services.nix-daemon = {
    environment.TMPDIR = "/var/tmp";
  };

  security.polkit.enable = true;
  programs.nix-ld.enable = true;
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
    optimise.automatic = true;
    settings = {
      substituters = [
        "https://fbegyn-personal.cachix.org/"
        "https://nix-community.cachix.org/"
        "https://cuda-maintainers.cachix.org/"
        "https://cache.lix.systems/"
        "https://cache.flox.dev/"
      ];
      trusted-public-keys = [
        "fbegyn-personal.cachix.org-1:0BEArpeI+ISsPainphPLHBozpP+zExYO6+43lLORDnI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
      ];
      trusted-users = [ "francis" ];
    };
    extraOptions = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes
    '' + lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    gc = {
      dates = "daily";
      automatic = true;
    };
    distributedBuilds = true;
    buildMachines = [
      {
        system = "x86_64-linux";
        hostName = "nix-builder-01";
        maxJobs = 4;
      }
    ];
  };

  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  # who uses nano anyways?
  programs.neovim = {
    enable = true;
    vimAlias = true;
    viAlias = true;
    defaultEditor = true;
  };

  home-manager.users.francis.home.stateVersion = lib.mkDefault "24.11";
  system.stateVersion = lib.mkDefault "24.11";

  age.secrets = {
    "secrets/passwords/mail/francsi".file = ../secrets/passwords/mail/francis.age;
    "secrets/passwords/mail/marc".file = ../secrets/passwords/mail/marc.age;
    "secrets/passwords/mail/dmarc".file = ../secrets/passwords/mail/dmarc.age;
    "secrets/passwords/mail/bots".file = ../secrets/passwords/mail/bots.age;
    "secrets/passwords/mail/robot".file = ../secrets/passwords/mail/robot.age;
    "secrets/passwords/mqtt/hass".file = ../secrets/passwords/mqtt/hass.age;
    "secrets/passwords/mqtt/shelly".file = ../secrets/passwords/mqtt/shelly.age;

    "secrets/api/cf".file = ../secrets/api/cf.age;
    "secrets/api/tailscale-temp".file = ../secrets/api/tailscale-temp.age;

    "secrets/services/alertmanager-env".file = ../secrets/services/alertmanager-env.age;

    "secrets/data/borgbase/key".file = ../secrets/data/borgbase/key.age;
    "secrets/data/borgbase/ssh".file = ../secrets/data/borgbase/ssh.age;
  };
}
