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
      unstable.picom
      unstable.i3status-rust
      feh
      i3lock
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
      "i3status-rs/config.toml".source = ./i3status-config.toml;
      "picom.conf".source = ./picom.conf;
    };
  };
}
