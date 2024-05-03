{ config, pkgs, lib, ... }:

let
  waybarsh = import ./waybar.sh.nix { inherit pkgs; };
  waybar-spotify = import ./waybar-spotify.nix { inherit pkgs; };
  waybar-storage = import ./waybar-storage.nix { inherit pkgs; };
  startriver = import ./startriver.nix { inherit pkgs; };
in
{
  imports = [
    ../../../roles/workstation/audio.nix
  ];

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  # fix clipboard for wayland
  nixpkgs.overlays = [
    (self: super: {
      wl-clipboard-x11 = super.stdenv.mkDerivation rec {
      pname = "wl-clipboard-x11";
      version = "5";

      src = super.fetchFromGitHub {
        owner = "brunelli";
        repo = "wl-clipboard-x11";
        rev = "v${version}";
        sha256 = "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb";
      };

      dontBuild = true;
      dontConfigure = true;
      propagatedBuildInputs = [ super.wl-clipboard ];
      makeFlags = [ "PREFIX=$(out)" ];
      };

      xsel = self.wl-clipboard-x11;
      xclip = self.wl-clipboard-x11;
    })
  ];

  environment.systemPackages = with pkgs; [
    river
    rivercarro
    foot

    waybar # status bar
    waybarsh
    waybar-spotify
    waybar-storage
    swaylock # lockscreen
    swayidle
    mako # notification daemon
    slurp
    grim
    imv
    xdg-utils
    wl-clipboard
    ydotool
    autotiling
    gammastep
    pkg-config
    wf-recorder
    kanshi # autorandr
    wdisplays
    brightnessctl
    fuzzel

    # polkit for the sway environment
    unstable.polkit_gnome

    # theming
    unstable.gtk-engine-murrine
    unstable.gtk_engines
    unstable.gsettings-desktop-schemas
    unstable.lxappearance
    unstable.glib
    unstable.gnome.adwaita-icon-theme
    unstable.libappindicator-gtk3
  ];
  services.udev.packages = with pkgs; [
    unstable.gnome.gnome-settings-daemon
  ];
  security.pam.services.swaylock = {};

  services.gnome.at-spi2-core.enable = true;

  services.dbus.enable = true;

  # more theming
  qt.platformTheme = "qt5ct";

  # polkit for the sway environment
  environment.pathsToLink = [ "/libexec" ];

  # for working tray applets
  environment.variables = {
    XDG_CURRENT_DESKTOP="unity";
  };


  home-manager.users.francis = {
    programs.fish = {
      loginShellInit = ''
        if test -z $DISPLAY; and test $XDG_VTNR -eq 2
          exec startriver
        end
      '';
    };
  };
}
