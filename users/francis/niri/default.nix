{ config, pkgs, ... }:

let
  niriwaybarsh = import ./waybar.sh.nix { inherit pkgs; };
  waybar-spotify = import ./waybar-spotify.nix { inherit pkgs; };
  waybar-storage = import ./waybar-storage.nix { inherit pkgs; };
in {
  imports = [
    ../../../common/pipewire.nix
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
  };

  xdg = {
    menus.enable = true;
    mime.enable = true;
    icons.enable = true;
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
      configPackages = [ pkgs.niri ];
    };
  };

  environment.systemPackages = with pkgs; [
    xdg-utils
    niri
    xwayland-satellite
    catppuccin-cursors.mochaMauve
    pkgs.waybar # status bar
    waybar-spotify
    waybar-storage
    swaylock # lockscreen
    swayidle
    swaybg
    mako # notification daemon
    slurp
    grim
    imv
    # unstable.flameshot.override { enableWlrSupport = true; }
    xdg-utils
    wl-clipboard
    ydotool
    autotiling
    gammastep
    pkgs.xdg-desktop-portal-wlr
    wl-mirror
    pipectl
    wofi
    rofi
    rofi-rbw-wayland
    # wf-recorder TODO: check when to fix
    kanshi # autorandr
    wdisplays
    brightnessctl
    pkg-config
    fuzzel

    # polkit for the sway environment
    unstable.polkit_gnome

    # theming
    unstable.gtk-engine-murrine
    unstable.gtk_engines
    unstable.gsettings-desktop-schemas
    unstable.lxappearance
    unstable.glib
    unstable.adwaita-icon-theme
    unstable.libappindicator-gtk3
  ];

  services.udev.packages = with pkgs; [
    unstable.gnome-settings-daemon
  ];

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  # more theming
  qt.platformTheme = "qt5ct";

  # for working tray applets
  environment.variables = {
    XDG_CURRENT_DESKTOP="sway";
    XDG_SESSION_DESKTOP="sway";
    XDG_SESSION_TYPE = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_QPA_PLATFORM = "wayland";
    QT_SCALE_FACTOR = "1";
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    MOZ_ENABLE_WAYLAND = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  # polkit for the sway environment
  environment.pathsToLink = [ "/libexec" ];

  services.displayManager.sessionPackages = [ pkgs.niri ];
  hardware.graphics.enable = true;

  services.gnome.gnome-keyring.enable = true;
  services.gnome.gcr-ssh-agent.enable = false;
  systemd.user.services.niri-flake-polkit = {
    description = "PolicyKit Authentication Agent provided by niri-flake";
    wantedBy = ["niri.service"];
    after = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  security.polkit = {
    enable = true;
    package = pkgs.polkit;
  };

  security.pam.services.swaylock = {};
  programs.dconf.enable = true;
  fonts.enableDefaultPackages = true;

  home-manager.users.francis = {
    programs.fish = {
      loginShellInit = ''
        if test -z $DISPLAY; and test $XDG_VTNR -eq 1
          exec niri-session
        end
      '';
    };
    home.packages = [
      pkgs.niri
      niriwaybarsh
    ];
    services.gnome-keyring.enable = true;

    services.flameshot = {
      enable = true;
      package = pkgs.unstable.flameshot.override { enableWlrSupport = true; };
    };

    programs.waybar = {
      enable = true;
      systemd = {
        enable = true;
      };
    };

    xdg.configFile = {
      "waybar/config".source = ./waybar-config;
      "waybar/style.css".source = ./waybar-style.css;
      "mako/config".source = ./mako-config;
      "chromium-flags.conf".text = ''
        --ozone-platform=wayland
      '';
      "wlr-config.ini".text = ''
        [preferred]
        # use xdg-desktop-portal-gtk for every portal interface
        default=gtk
        # except for the xdg-desktop-portal-wlr supplied interfaces
        org.freedesktop.impl.portal.Screencast=wlr
        org.freedesktop.impl.portal.Screenshot=wlr

        [screencast]
        chooser_cmd=swaymsg -t get_outputs | jq '.[] | .name' | sed 's/"//g' | wofi -d
        chooser_type=dmenu
        max_fps=30
      '';
    };
    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
      configPackages = [ pkgs.niri ];
    };
  };
}
