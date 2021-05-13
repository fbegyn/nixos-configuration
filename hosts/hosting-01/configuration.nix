# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
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
      ../../services/website
      ../../services/tailscale.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "hosting-01"; # Define your hostname.
  time.timeZone = "Europe/Brussels"; # Set your time zone.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.ens10.useDHCP = true;
  networking.interfaces.ens3.useDHCP = true;
  networking.defaultGateway6 = {
    address = "fe80::1";
    interface = "ens3";
  };
  networking.interfaces.ens3.ipv6 = {
    addresses = [
      {
        address = "2a01:4f9:c010:d553::1";
        prefixLength = 64;
      }
    ];
  };

  # tailscale machine specific
  # create a oneshot job to authenticate to Tailscale
  systemd.services.tailscale-autoconnect = let
    hosts = import ../../secrets/hosts.nix;
  in {
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
      ${tailscale}/bin/tailscale up -authkey ${hosts.hosting-01.tailscale.oneoffkey}
    '';
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = false;
  };

  networking.firewall.interfaces = {
    "tailscale0" = {
      allowedTCPPorts = [ 22 ];
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

