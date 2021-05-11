# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
    hosts = import ../../secrets/hosts.nix;
    vars = hosts.mail-01;
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../../common/base.nix
    ../../common/master.nix
    ../../common/unstable.nix
    ../../common/security.nix
    ../../common/system.nix
    ../../common/resolved.nix

    ../../users
    ../../users/francis

    ../../common/acme.nix
    ../../services/tailscale.nix

    # simple mail server
    (builtins.fetchTarball {
      # Pick a commit from the branch you are interested in
      url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/8b287056215cac91438a671054e7eb2c932ab21a/nixos-mailserver-8b287056215cac91438a671054e7eb2c932ab21a.tar.gz";
      # And set its hash
      sha256 = "1h1x2498j5ki6pajsbgwq664j9isihzgjsg87fgqdzyizrxn0mai";
    })
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  boot.initrd.supportedFilesystems = ["zfs"]; # boot from zfs
  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "mail-01"; # Define your hostname.
  networking.hostId = vars.hostId;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  # tailscale machine specific
  # create a oneshot job to authenticate to Tailscale
  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";

    # make sure tailscale is running before trying to connect to tailscale
    after = [ "network-pre.target" "tailscaled.service" ];
    wants = [ "network-pre.target" "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];

    # set this service as a oneshot job
    serviceConfig.Type = "oneshot";

    # have the job run this shell script
    script = with pkgs.unstable; ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${tailscale}/bin/tailscale status -json | ${jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${tailscale}/bin/tailscale up -authkey ${vars.tailscale.oneoffkey}

    '';
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  # Fail2ban
  services.fail2ban = {
    enable = true;
    jails = {
      dovecot = ''
        enabled  = true
        port     = pop3,pop3s,imap,imap
        filter   = dovecot
        maxretry = 3
      '';
      postfix = ''
        enabled  = true
        port     = smtp, ssmtp
        filter   = postfix
        maxretry = 5
      '';
    };
  };

  networking.firewall.interfaces = {
    "tailscale0" = {
      allowedTCPPorts = [ 22 ];
    };
  };

  security.acme.email = vars.acme.email;
  mailserver = {
    enable = true;
    fqdn = vars.mailserver.fqdn;
    domains = vars.mailserver.domains;
    loginAccounts = vars.mailserver.accounts;
    certificateScheme = 3;
    virusScanning = true;
    monitoring = {
      enable = false;
      alertAddress = vars.mailserver.alertAddress;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

