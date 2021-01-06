# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../../nixos-hardware/dell/xps/13-9380
    ../../secrets/wireless.nix
    #../../secrets/eos-wireguard.nix
    ../../common/base.nix
    ../../common/security.nix
    ../../common/pulseaudio.nix
    ../../common/screen-brightness.nix
    ../../common/bluetooth.nix
    ../../common/fonts.nix
    ../../common/printer.nix
    ../../common/wireguard.nix
    ../../common/kubernetes.nix
    ../../common/openvpn.nix
    ../../users
    ../../users/francis
    ../../users/francis/configurations/i3
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.extraModulePackages = [
    (pkgs.callPackage ../../common/hid-apple-patched.nix {
      kernel = pkgs.linuxPackages_latest.kernel;
    })
  ];
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
    options hid_apple swap_fn_leftctrl=0
    options hid_apple swap_opt_cmd=1
  '';

  networking.hostName = "ania"; # After the Greek titan of dawn
  networking.wireless = {
    enable = true;
    interfaces = [ "wlp0s20f3" ];
  };
  networking.extraHosts = ''
    172.22.21.16  hhtperf minio1.hhtperf minio2.hhtperf devpi.hhtperf dashboard.hhtperf files.hhtperf ray.hhtperf prometheus.hhtperf grafana.hhtperf
  '';

  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  #networking.interfaces.enp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.adb.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    pinentryFlavor = "gtk2";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;
  services.hardware.bolt.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  networking.wireguard = {
    enable = true;
  };

  virtualisation.docker.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
