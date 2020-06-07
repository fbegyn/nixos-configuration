{ pkgs, ... }:

{
  imports = [
    ../dunst
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.windowManager.i3.enable = true;

  home-manager.users.francis = {
    home.packages = with pkgs; [
      maim
      xclip
      picom
      i3
      feh
    ];

    home = {
      file.".config/i3/config".source = ./config;
    };
  };
}
