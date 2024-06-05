{ config, pkgs, ... }: {
  imports = [
    ../../common/bluetooth.nix
    ../../common/network-tools.nix
    ../../common/wireguard.nix
    ./audio.nix
    ./fonts.nix
  ];

  boot.supportedFilesystems = [ "zfs" "ntfs" ];
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "nl_BE.UTF-8";
    supportedLocales = [
      "nl_BE.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LC_MESSAGES = "en_US.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Brussels";

  # packages
  environment.systemPackages = with pkgs.unstable; [
    pkgs.pinentry-gtk2
    # office
    libreoffice-fresh
    # networkmanager
    networkmanagerapplet
    libnma
    # webcam
    guvcview
    ffmpeg
    v4l-utils
    # ios
    libimobiledevice
    ifuse
  ];

  # networkmanager
  programs = {
    nm-applet.enable = true;
  };
  # networking.networkmanager.packages = with pkgs.unstable; [
  # ];

  # ios connection
  services.usbmuxd = {
    enable = true;
  };

  # gpg
  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gtk2;
  };

  # enable video acceleration
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
    driSupport32Bit = true;
  };

  home-manager.users.francis.home.stateVersion = "23.11";
}
