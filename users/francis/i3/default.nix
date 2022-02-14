{ pkgs, ... }:

{
  imports = [
    ./dunst.nix
  ];

  environment.pathsToLink = [ "/libexec" ];

  # Enable the X11 windowing system.
  services.xserver= {
    enable = true;
    layout = "us,us";
    xkbVariant = "altgr-intl,colemak";
    xkbOptions = "eurosign:5,grp:win_space_toggle";
    libinput.enable =true;
  };

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.unstable.i3;
    extraPackages = with pkgs.unstable; [
      maim
      pkg-config
      xclip
      arandr
      autorandr
      rofi
      rofi-pass
      picom
      feh
      i3lock
      i3status-rust
      xkb-switch
      betterlockscreen
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
      "i3status/i3status.conf".source = ./i3status.conf;
      "i3status-rs/config.toml".source = ./i3status-config.toml;
      "picom.conf".source = ./picom.conf;
      "rofi/config.rasi".source = ./config.rasi;
    };
  };
}
