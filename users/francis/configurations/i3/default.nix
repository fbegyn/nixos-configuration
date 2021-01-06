{ pkgs, ... }:

{
  imports = [
    ../dunst
  ];

  # Enable the X11 windowing system.
  services.xserver= {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "eurosign:5";
  };

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.unstable.i3;
    extraPackages = with pkgs; [
      maim
      xclip
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
    };
  };
}
