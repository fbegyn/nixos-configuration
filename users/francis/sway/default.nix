{ config, pkgs, lib, ... }:

let
  waybarsh = import ./waybar.sh.nix { inherit pkgs; };
  waybar-spotify = import ./waybar-spotify.nix { inherit pkgs; };
  waybar-storage = import ./waybar-storage.nix { inherit pkgs; };
  startsway = import ./startsway.nix { inherit pkgs; };
in
{
  imports = [
    ../../../common/pipewire.nix
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures = {
      base = true;
      gtk = true;
    };
    extraPackages = with pkgs.unstable; [
      # waybar + scripts
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
      swayr
      wl-clipboard
      ydotool
      autotiling
      gammastep
      pkg-config
      wf-recorder
      kanshi # autorandr
      wdisplays
      brightnessctl
    ];
  };
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
    startsway
    wl-clipboard
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

  services.dbus.enable = true;

  # more theming
  qt5.platformTheme = "qt5ct";

  # polkit for the sway environment
  environment.pathsToLink = [ "/libexec" ];

  # for working tray applets
  environment.variables = {
    XDG_CURRENT_DESKTOP="unity";
  };

  # use gdm as display manager
  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.displayManager.sddm.enable = true;

  # sway install and dependencies through home-manager
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.wofi
      unstable.fuzzel
    ];

    xdg.configFile = {
      "mako/config".source = ./mako-config;
      "waybar/config".source = ./waybar-config;
      "waybar/style.css".source = ./waybar-style.css;
    };

    programs.fish = {
      loginShellInit = ''
        systemctl --user import-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK
        if test -z $DISPLAY; and test $XDG_VTNR -eq 1
          exec startsway
        end
      '';
    };

    wayland.windowManager.sway = let
      term = config.home-manager.users.francis.wayland.windowManager.sway.config.terminal;
    in {
      enable = true;
      xwayland = true;
      systemdIntegration = true;
      wrapperFeatures.gtk = true;
      extraSessionCommands = ''
        export XDG_SESSION_TYPE=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
        export QT_AUTO_SCREEN_SCALE_FACTOR=0
        export QT_SCALE_FACTOR=1
        export GDK_SCALE=1
        export GDK_DPI_SCALE=1
        export MOZ_ENABLE_WAYLAND=1
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
      config = {
        fonts = {
          names = [ "Hack" "DejaVu Sans Mono" "FontAwesome" ];
          size = 10.0;
        };
        terminal = "alacritty";
        menu = "${pkgs.unstable.fuzzel}/bin/fuzzel";
        modifier = "Mod4";
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = {
            xkb_layout = "us,us";
            xkb_variant = "altgr-intl,colemak";
            xkb_numlock = "disabled";
            xkb_options = "grp:rctrl_rshift_toggle";
          };
          "2:10:TPPS/2_Elan_TrackPoint" = {
            pointer_accel = "-0.17";
          };
          "1133:49971:Logitech_Gaming_Keyboard_G610" = {
            xkb_layout = "us";
            xkb_variant = "altgr-intl";
            xkb_numlock = "enabled";
          };
          "1133:49971:Logitech_Gaming_Keyboard_G610_Keyboard" = {
            xkb_layout = "us";
            xkb_variant = "altgr-intl";
            xkb_numlock = "enabled";
          };
        };
        assigns = {
          "1:web" = [{ class = "^chromium-browser$"; } { class = "^qutebrowser$"; }];
          "2:comms" = [
            { class = "^Slack$"; }
            { class = "^Mattermost$"; }
            { class = "^Telegram$"; }
            { class = "^Caprine$"; }
          ];
          "3:mail" = [{ class = "^Thunderbird$"; }];
          "4:music" = [{ class = "^Spotify"; }];
        };
        bars = [];
        floating = {
          border = 2;
          criteria = [ ];
          modifier = "Mod4";
          titlebar = true;
        };
        gaps = {
          smartGaps = true;
          inner = 0;
          outer = 2;
        };
        focus = {
          followMouse = "yes";
          forceWrapping = false;
          mouseWarping = true;
          newWindow = "smart";
        };
        up = "k";
        down = "j";
        right = "l";
        left = "h";
        keybindings =
          let
            wm = config.home-manager.users.francis.wayland.windowManager.sway;
            mod = wm.config.modifier;
            terminal = wm.config.terminal;
            locker = "swaylock -c 000000";
          in {
          "${mod}+Return" = "exec ${wm.config.terminal}";
          "${mod}+Shift+q" = "kill";
          "${mod}+d" = "exec ${wm.config.menu}";

          "${mod}+${wm.config.left}" = "focus left";
          "${mod}+${wm.config.down}" = "focus down";
          "${mod}+${wm.config.up}" = "focus up";
          "${mod}+${wm.config.right}" = "focus right";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          "${mod}+Shift+${wm.config.left}" = "move left";
          "${mod}+Shift+${wm.config.down}" = "move down";
          "${mod}+Shift+${wm.config.up}" = "move up";
          "${mod}+Shift+${wm.config.right}" = "move right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          "${mod}+b" = "splith";
          "${mod}+v" = "splitv";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+a" = "focus parent";

          "${mod}+s" = "layout stacking";
          "${mod}+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";

          "Mod1+l" = "exec --no-startup-id ${locker}";

          "${mod}+Shift+space" = "floating toggle";
          "${mod}+space" = "focus mode_toggle";

          "${mod}+1" = "workspace number 1:web";
          "${mod}+2" = "workspace number 2:comms";
          "${mod}+3" = "workspace number 3:mail";
          "${mod}+4" = "workspace number 4:music";
          "${mod}+5" = "workspace number 5:video";
          "${mod}+6" = "workspace number 6:ide";
          "${mod}+7" = "workspace number 7:files";
          "${mod}+8" = "workspace number 8:workspace";
          "${mod}+9" = "workspace number 9:terminal";

          "${wm.config.modifier}+Shift+1" =
            "move container to workspace number 1:web";
          "${wm.config.modifier}+Shift+2" =
            "move container to workspace number 2:comms";
          "${wm.config.modifier}+Shift+3" =
            "move container to workspace number 3:mail";
          "${wm.config.modifier}+Shift+4" =
            "move container to workspace number 4:music";
          "${wm.config.modifier}+Shift+5" =
            "move container to workspace number 5:video";
          "${wm.config.modifier}+Shift+6" =
            "move container to workspace number 6:ide";
          "${wm.config.modifier}+Shift+7" =
            "move container to workspace number 7:files";
          "${wm.config.modifier}+Shift+8" =
            "move container to workspace number 8:workspace";
          "${wm.config.modifier}+Shift+9" =
            "move container to workspace number 9:terminal";

          "${wm.config.modifier}+Shift+minus" = "move scratchpad";
          "${wm.config.modifier}+minus" = "scratchpad show";

          "${wm.config.modifier}+Shift+c" = "reload";
          "${wm.config.modifier}+Shift+f" =
            "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";

          "${mod}+r" = "mode resize";

          "${mod}+Tab" = "exec swayr switch-window";
        };
        keycodebindings = { };
        modes = let
            wm = config.home-manager.users.francis.wayland.windowManager.sway;
        in {
          resize = {
            Escape = "mode default";
            Return = "mode default";
            "${wm.config.left}" = "resize shrink width 10 px";
            "${wm.config.up}" = "resize grow height 10 px";
            "${wm.config.down}" = "resize shrink height 10 px";
            "${wm.config.right}" = "resize grow width 10 px";
          };
          "System (l)lock,(e)logout,(s)suspend,(r)reboot,(Shift+s)shutdown" = {
            Return = "mode default";
            Escape = "mode default";
            l = "exec --no-startup-id swaylock -c 000000, mode default";
            e = "exec --no-startup-id i3-msg exit, mode default";
            s = "exec --no-startup-id systemctl suspend, mode default";
            r = "exec --no-startup-id systemctl reboot, mode default";
            "Shift+s" = "exec --no-startup-id systemctl poweroff -i, mode default";
          };
        };
        startup = [
          # {
          #   command = "qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=2";
          # }
          {
            command = "chromium-browser";
          }
          {
            command = "swayrd";
          }
          {
            command = "sway-statusbar.sh";
            always = true;
          }
          {
            command = "gammastep -l 51.038292:3.712173";
          }
          {
            command = "autotiling";
          }
          {
            command = "mako";
          }
          {
            command = "nm-applet --indicator";
          }
        ];
        window = {
          border = 2;
          commands = [
            {
                command = "move to workspace 4:music";
                criteria = { app_id = "Spotify"; };
            }
            {
                command = "inhibit_idle visible";
                criteria = { app_id = "mpv"; };
            }
            {
                command = "floating enable";
                criteria = { app_id = "pavucontrol"; };
            }
          ];
          hideEdgeBorders = "none";
          titlebar = false;
        };
        workspaceAutoBackAndForth = true;
        workspaceLayout = "default";
      };
      extraConfig =
        let
          wm = config.home-manager.users.francis.wayland.windowManager.sway;
          mod = wm.config.modifier;
      in ''
        # set wallpaper
        output "*" background ~/Pictures/wallpapers/background.jpg fill

        # monitor config
        set $laptop_display eDP-1
        output $laptop_display pos 0 0 res 1920x1080

        # hide cursor after time
        seat seat0 hide_cursor 2500

        # set global modifier to windows key
        set $mod ${mod}

        set $locker swaylock -c 000000

        # Hide border if only 1 window
        hide_edge_borders smart

        # color scheme
        client.focused              #d75f00	#1c1b19	#ffffff	#2e9ef4	#ff8700
        client.focused_inactive	    #333333	#5f676a	#ffffff	#484e50	#5f676a
        client.unfocused	          #333333	#1c1b19	#888888	#292d2e	#222222
        client.urgent	              #f75341	#ef2f27	#1c1b19	#900000	#f75341
        client.placeholder	        #000000	#0c0c0c	#ffffff	#000000	#0c0c0c

        # Use Mouse+$mod to drag floating windows to their wanted position
        floating_modifier $mod

        # Set terminal
        set $terminal alacritty

        # split the window
        # change container layout (stacked, tabbed, toggle split)
        bindsym $mod+z layout tabbed

        # Workspace bindings
        set $workspace1 1:web
        set $workspace2 2:comms
        set $workspace3 3:mail
        set $workspace4 4:music
        set $workspace5 5:video
        set $workspace6 6:ide
        set $workspace7 7:files
        set $workspace8 8:workplace
        set $workspace9 9:terminal

        # reload the configuration file
        bindsym $mod+Shift+r reload

        #System menu
        set $mode_system System (l)lock,(e)logout,(s)suspend,(r)reboot,(Shift+s)shutdown
        bindsym $mod+Pause mode "$mode_system"

        # idle config
        # Idle configuration
        exec swayidle -w \
            timeout 60 'exec $locker -f' \
            timeout 120 'swaymsg "output dpms * off"' \
              resume 'swaymsg "output * dpms on"' \
            before-sleep 'exec $locker -f' \
            after-resume 'swaymsg "output * dpms on"'

        # disable laptop output on lid close
        # enabling this, then I should disabl the logind handlers
        # bindswitch --reload lid:on output $laptop_display disable
        # bindswitch --reload lid:off output $laptop_display enable

        # Screenshot menu
        set $screen_grab s/f clipboard, Shift+s/f local, Alt+s/f Imgur, m recorder
        mode "$screen_grab" {
            bindsym s exec 'grim -t png -g "$(slurp -d)" - | wl-copy -t image/png', mode "default"
            bindsym Shift+s exec 'grim -t png -g "$(slurp -d)" ~/Pictures/Screenshots/$(date +%F-%T).png', mode "default"
            bindsym Mod1+s exec 'grim -t png -g "$(slurp -d)" - | ~/Scripts/imgur.sh', mode "default"
            bindsym f exec 'grim -t png - | wl-copy -t image/png', mode "default"
            bindsym Shift+f exec 'grim -t png ~/Pictures/Screenshots/$(date +%F-%T).png', mode "default"
            bindsym Mod1+f exec 'grim -t png - | ~/Scripts/imgur.sh', mode "default"
            bindsym m exec 'wf-recorder -g $(slurp -d) -f ~/Videos/Captures/$(date +%Y-%m-%d-%H:%M:%S).mp4', mode "default"
            # back to normal: Enter or Escape
            bindsym Return mode "default"
            bindsym Escape mode "default"
        }
        bindsym $mod+grave mode "$screen_grab"

        # Make the current window stick acroos workspaces
        bindsym $mod+Shift+s sticky toggle

        # Tiny sticky window
        bindsym $mod+y floating toggle; resize set 424 212; sticky toggle; move window to position 1490 5;
        for_window [title="yt-player"] floating_minimum_size 320x200; floating_maximum_size 320x200;
        for_window [app_id="^launcher$"] floating enable, sticky enable, resize set 384 px 512 px, border pixel 5

        # Immediately play youtube from rofi output
        bindsym $mod+p exec rofi-pass

        # Reload monitor config
        bindsym $mod+Shift+m exec --no-startup-id /home/francis/Scripts/monitor-hotplug.sh

        # Volume control
        bindsym XF86AudioLowerVolume exec pulsemixer --change-volume -2
        bindsym XF86AudioRaiseVolume exec pulsemixer --change-volume +2
        bindsym XF86AudioMute exec pulsemixer --toggle-mute

        # Media control
        bindsym Shift+XF86AudioPlay exec mpc toggle # Mopidy bindings
        bindsym Shift+XF86AudioNext exec mpc next
        bindsym Shift+XF86AudioPrev exec mpc prev
        bindsym XF86AudioPlay exec playerctl play-pause # Spotify bindings
        bindsym XF86AudioNext exec playerctl next
        bindsym XF86AudioPrev exec playerctl previous

        # Brightness control
        bindsym XF86MonBrightnessDown exec --no-startup-id brightnessctl -e s 10%-
        bindsym XF86MonBrightnessUp exec --no-startup-id brightnessctl -e s +10%

        # Handle the slow startup of GTK issues
        # see https://github.com/swaywm/sway/wiki#gtk-applications-take-20-seconds-to-start
        exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
      '';
    };
  };
}
