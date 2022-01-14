{ pkgs, ... }:

{
  imports = [
    ../dunst
  ];

  environment.pathsToLink = [ "/libexec" ];

  # Enable the X11 windowing system.
  services.xserver= {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "eurosign:5";
    libinput.enable =true;
  };

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3;
    extraPackages = with pkgs; [
      maim
      pkg-config
      xclip
      arandr
      autorandr
      rofi
      rofi-pass
      unstable.picom
      feh
      i3lock
      unstable.betterlockscreen
    ];
  };

  home-manager.users.francis = {
    services = {
        unclutter = {
        enable = true;
        timeout = 5;
        };
    };

    xdg.configFile = {
      "i3/config".source = ./config;
      "picom.conf".source = ./picom.conf;
      "rofi/config.rasi".source = ./config.rasi;
    };
  };
}
