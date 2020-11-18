{ config, pkgs, lib, ... }:

let
  waybarsh = import ./waybar.sh.nix { inherit pkgs; };
  waybar-spotify = import ./waybar-spotify.nix { inherit pkgs; };
  waybar-storage = import ./waybar-storage.nix { inherit pkgs; };
  startsway = import ./startsway.nix { inherit pkgs; };
in
{
  programs.waybar.enable = true;

  hardware.opengl.enable = true;

  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.swaylock # lockscreen
      unstable.swayidle
      wf-recorder
      xwayland # for legacy apps
      unstable.waybar # status bar
      unstable.mako # notification daemon
      kanshi # autorandr
      libappindicator-gtk3
      waybarsh
      waybar-spotify
      waybar-storage
      startsway
      unstable.slurp
      unstable.grim
      unstable.wl-clipboard
      wdisplays
      unstable.wofi
    ];

    services.redshift = {
      package = pkgs.unstable.redshift-wlr;
    };

    xdg.configFile = {
      "mako/config".text = ''
        anchor=top-center
        default-timeout=2000
      '';
      "waybar/config".source = ./waybar-config;
      "waybar/style.css".source = ./waybar-style.css;
    };

    wayland.windowManager.sway = {
      enable = true;
      systemdIntegration = true;
      extraSessionCommands = ''
        systemctl --user import-environment
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
        assigns = {
          "1:web" = [{ class = "^qutebrowser$"; }];
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
        menu = "rofi -show drun";
        fonts = [ "DejaVu Sans Mono 10" ];
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = {
            xkb_layout = "us";
            xkb_variant = "altgr-intl";
            xkb_numlock = "disabled";
          };   
          "2:10:TPPS/2_Elan_TrackPoint" = {
            pointer_accel = "-0.17";
          };
        };
        keybindings =
          let
            wm = config.home-manager.users.francis.wayland.windowManager.sway;
            mod = wm.config.modifier;
            terminal = wm.config.terminal;
            menu = wm.config.menu;
          in
            {
              "${mod}+Return" = "exec ${terminal}";
              "${mod}+Shift+Return" = "exec ${terminal} -e tmux new-session -A main";
        };
        keycodebindings = { };
        modes = {
          resize = {
            Escape = "mode default";
            Return = "mode default";
            h = "resize shrink width 10 px";
            j = "resize grow height 10 px";
            k = "resize shrink height 10 px";
            l = "resize grow width 10 px";
          };
        };
        modifier = "Mod4";
        startup = [
          {
            command = "qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=2";
          }
          {
            command = "sway-statusbar.sh";
            always = true;
          }
        ];
        terminal = "alacritty";
        up = "k";
        down = "j";
        right = "l";
        left = "h";
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
          titlebar = true;
        };
        workspaceAutoBackAndForth = true;
        workspaceLayout = "default";
      };
      extraConfig = ''
        xwayland enable
        # set wallpaper
        output "*" background ~/Pictures/wallpapers/background.jpg fill
        # monitor config
        set $laptop_display eDP-1
        output $laptop_display pos 0 1080 res 1920x1080
        # hide cursor after time
        # seat seat0 hide_cursor 2500
        # set global modifier to windows key
        set $mod Mod4
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
        # start a terminal
        # kill focused window
        bindsym $mod+Shift+q kill
        bindsym $mod+a exec rofi -show window
        # change focus
        bindsym $mod+h focus left
        bindsym $mod+j focus down
        bindsym $mod+k focus up
        bindsym $mod+l focus right
        # alternatively, you can use the cursor keys:
        bindsym $mod+Left focus left
        bindsym $mod+Down focus down
        bindsym $mod+Up focus up
        bindsym $mod+Right focus right
        # move focused window
        bindsym $mod+Shift+h move left
        bindsym $mod+Shift+j move down
        bindsym $mod+Shift+k move up
        bindsym $mod+Shift+l move right
        # split the window
        bindsym $mod+b splith
        bindsym $mod+v splitv
        # change container layout (stacked, tabbed, toggle split)
        bindsym $mod+e layout toggle split
        bindsym $mod+s layout stacking
        bindsym $mod+z layout tabbed
        # set window to fullscreen
        bindsym $mod+f fullscreen
        # toggle tiling / floating
        bindsym $mod+Shift+space floating toggle
        # change focus between tiling / floating windows
        bindsym $mod+space focus mode_toggle
        # Workspace bindings
        set $workspace1 1:web
        set $workspace2 2:comms
        set $workspace3 3:mail
        set $workspace4 4:music
        set $workspace5 5:video
        set $workspace6 6:ide
        set $workspace7 7:files
        set $workspace8 8
        set $workspace9 9:terminal
        set $workspace10 0
        # switch to workspace
        bindsym $mod+1 workspace $workspace1
        bindsym $mod+2 workspace $workspace2
        bindsym $mod+3 workspace $workspace3
        bindsym $mod+4 workspace $workspace4
        bindsym $mod+5 workspace $workspace5
        bindsym $mod+6 workspace $workspace6
        bindsym $mod+7 workspace $workspace7
        bindsym $mod+8 workspace $workspace8
        bindsym $mod+9 workspace $workspace9
        bindsym $mod+0 workspace $workspace10
        # move focused container to workspace
        bindsym $mod+Shift+1 move container to workspace $workspace1
        bindsym $mod+Shift+2 move container to workspace $workspace2
        bindsym $mod+Shift+3 move container to workspace $workspace3
        bindsym $mod+Shift+4 move container to workspace $workspace4
        bindsym $mod+Shift+5 move container to workspace $workspace5
        bindsym $mod+Shift+6 move container to workspace $workspace6
        bindsym $mod+Shift+7 move container to workspace $workspace7
        bindsym $mod+Shift+8 move container to workspace $workspace8
        bindsym $mod+Shift+9 move container to workspace $workspace9
        bindsym $mod+Shift+0 move container to workspace $workspace10
        # move workspace across monitors
        bindsym Mod1+Shift+h move workspace to output right
        bindsym Mod1+Shift+j move workspace to output down
        bindsym Mod1+Shift+k move workspace to output up
        bindsym Mod1+Shift+l move workspace to output left
        # reload the configuration file
        bindsym $mod+Shift+r reload
        # exit i3 (logs you out of your X session)
        bindsym $mod+Shift+e exec "swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your sway session.' -b 'Yes, exit sway' 'swaymsg exit'"
        set $locker swaylock -c 000000
        #System menu
        set $mode_system System (l)lock,(e)logout,(s)suspend,(r)reboot,(Shift+s)shutdown
        mode "$mode_system" {
            bindsym l exec --no-startup-id $locker, mode "default"
            bindsym e exec --no-startup-id i3-msg exit, mode "default"
            bindsym s exec --no-startup-id systemctl suspend, mode "default"
            bindsym r exec --no-startup-id systemctl reboot, mode "default"
            bindsym Shift+s exec --no-startup-id systemctl poweroff -i, mode "default"
            # back to normal: Enter or Escape
            bindsym Return mode "default"
            bindsym Escape mode "default"
        }
        bindsym $mod+Pause mode "$mode_system"
        #bindswitch lid:on exec systemctl suspend
        # idle config
        # Idle configuration
        exec swayidle -w \
        timeout 60 'exec $locker -f' \
        timeout 360 'swaymsg "output dpms * off"' \
                resume 'swaymsg "output dpms * on"' \
        timeout 1800 'systemctl suspend' \
                resume 'swaymsg "output dpms * on"' \
        before-sleep 'exec $locker -f' \
        after-resume 'swaymsg "output dpms * on"'
        # disable laptop output on lid close
        # enabling this, then I should disabl the logind handlers
        bindswitch --reload lid:on output $laptop_display disable
        bindswitch --reload lid:off output $laptop_display enable
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
        # Immediately play youtube from rofi output
        bindsym $mod+p exec rofi-pass
        # Reload monitor config
        bindsym $mod+Shift+m exec --no-startup-id /home/francis/Scripts/monitor-hotplug.sh
        # Screen lock
        bindsym Mod1+l exec $locker
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
        # Bind wifi switching script
        bindsym $mod+w exec /home/francis/Scripts/wpa_switcher.sh
      '';
    };
  };
}
