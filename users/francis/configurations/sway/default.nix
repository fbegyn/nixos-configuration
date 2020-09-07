{ config, pkgs, lib, ... }:

let
  waybarsh = import ./waybar.sh.nix { inherit pkgs; };
  waybar-spotify = import ./waybar-spotify.nix { inherit pkgs; };
  waybar-storage = import ./waybar-storage.nix { inherit pkgs; };
  startsway = import ./startsway.nix { inherit pkgs; };
in
{
  home-manager.users.francis.services.redshift = {
    package = pkgs.unstable.redshift-wlr;
  };

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
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
    ];
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
  };

  systemd.user = {
    targets.sway-session = {
      description = "Sway compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };

    services = {
      sway = {
        description = "Sway - Wayland window manager";
        documentation = [ "man:sway(5)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
        environment.PATH = lib.mkForce null;
        serviceConfig = {
          Type = "simple";
          ExecStart = ''
            ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
          '';
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
      kanshi = {
        description = "Kanshi output autoconfig ";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        serviceConfig = {
          # kanshi doesn't have an option to specifiy config file yet, so it looks
          # at .config/kanshi/config
          ExecStart = ''
            ${pkgs.kanshi}/bin/kanshi
          '';
          RestartSec = 5;
          Restart = "always";
        };
      };
    };
  };

  programs.waybar.enable = true;


  home-manager.users.francis = {
    home.packages = with pkgs; [
    ];
    programs.fish = {
      loginShellInit = ''
        startsway
      '';
    };
    xdg.configFile = {
      "waybar/config".source = ./waybar-config;
      "waybar/style.css".source = ./waybar-style.css;

      "sway/config".text = ''
        xwayland enable
        # set wallpaper
        # output "*" background ~/Pictures/Wallpapers/amid-clouds.jpg fill
        # monitor config
        set $laptop_display eDP-1
        output $laptop_display pos 0 1080 res 1920x1080
        # configure inputs
        #input 1:1:AT_Translated_Set_2_keyboard {
        #    xkb_layout us
        #    xkb_variant altgr-intl
        #    xkb_numlock disabled
        #  }
        #
        #input 1452:570:Toetsenbord_van_Tekenkamer {
        #    xkb_layout us
        #    xkb_variant altgr-intl
        #    xkb_numlock disabled
        #  }
        # hide cursor after time
        # seat seat0 hide_cursor 2500
        # set global modifier to windows key
        set $mod Mod4
        # Set the default gaps size for windows
        gaps inner 1
        gaps outer 1
        # Only do gaps when more then 1 container is on the workspace
        smart_gaps on
        # Disable the annoying titlebars
        default_border pixel 2
        default_floating_border pixel 3
        # Hide border if only 1 window
        hide_edge_borders smart
        # color scheme
        client.focused              #d75f00	#1c1b19	#ffffff	#2e9ef4	#ff8700
        client.focused_inactive	    #333333	#5f676a	#ffffff	#484e50	#5f676a
        client.unfocused	          #333333	#1c1b19	#888888	#292d2e	#222222
        client.urgent	              #f75341	#ef2f27	#1c1b19	#900000	#f75341
        client.placeholder	        #000000	#0c0c0c	#ffffff	#000000	#0c0c0c
        # Font for window titles. Will also be used by the bar unless a different font
        # is used in the bar {} block below.
        #font pango:monospace 8
        # This font is widely installed, provides lots of unicode glyphs, right-to-left
        # text rendering and scalability on retina/hidpi displays (thanks to pango).
        font pango:DejaVu Sans Mono 8
        # Use Mouse+$mod to drag floating windows to their wanted position
        floating_modifier $mod
        # Set terminal
        set $terminal alacritty
        # start a terminal
        bindsym $mod+Shift+Return exec $terminal
        bindsym $mod+Return exec $terminal -e tmux new-session -A -s main
        # kill focused window
        bindsym $mod+Shift+q kill
        # start dmenu (a program launcher)
        bindsym $mod+d exec rofi -show drun
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
        # resize window (you can also use the mouse for that)
        set $size_step 2 px or 2 ppt
        mode "resize" {
            # These bindings trigger as soon as you enter the resize mode
            # Pressing left will shrink the window’s width.
            # Pressing right will grow the window’s width.
            # Pressing up will shrink the window’s height.
            # Pressing down will grow the window’s height.
            bindsym h resize shrink width $size_step
            bindsym k resize grow height $size_step
            bindsym j resize shrink height $size_step
            bindsym l resize grow width $size_step
            # same bindings, but for the arrow keys
            bindsym Left resize shrink width $size_step
            bindsym Down resize grow height $size_step
            bindsym Up resize shrink height $size_step
            bindsym Right resize grow width $size_step
            # back to normal: Enter or Escape
            bindsym Return mode "default"
            bindsym Escape mode "default"
        }
        bindsym $mod+r mode "resize"
        #
        # Status bar:
        #
        exec_always sway-statusbar.sh
        #bar {
        #				font pango:DejaVu Sans Mono, FontAwesome 9
        #        status_command ${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rs/config.toml
        #        position top
        #}
        # Set the locker variable
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
        # Autolocker
        #exec_always --no-startup-id xautolock
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
        # Autoswitch workspaces
        workspace_auto_back_and_forth yes
        # auto set workspaces
        #exec --no-startup-id i3-msg "workspace $workspace1; append_layout /home/francis/.config/i3/workspaces/web.json"
        #exec --no-startup-id $terminal --class 'web_terminal' -t web_terminal -e tmux
        # Autostart programs
        #exec --no-startup-id vivaldi-snapshot
        exec --no-startup-id qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=2
        #exec --no-startup-id slack
        #exec --no-startup-id telegram-desktop
        #exec --no-startup-id thunderbird
        #exec --no-startup-id spotify
        # Asign programs to workspaces
        assign [class="Hamsket"] $workspace2
        assign [class="Slack"] $workspace2
        assign [class="Mattermost"] $workspace2
        assign [class="Caprine"] $workspace2
        assign [class="Telegram"] $workspace2
        assign [class="Thunderbird"] $workspace3
        assign [class="Mailspring"] $workspace3
        assign [class="Vivaldi-stable"] $workspace1
        assign [class="qutebrowser"] $workspace1
        #assign [class="Firefox"] $workspace1
        assign [app_id="Spotify"] $workspace4
        for_window [app_id="Spotify"] move to workspace $workspace4
        # Set some apps to be floating
        for_window [app_id="pavucontrol"] floating enable
        for_window [app_id="Nautilus"] floating enable
        for_window [app_id="blueberry.py"] floating enable
        for_window [app_id="Nmtui"] floating enable
        for_window [app_id="Pulsemixer"] floating enable
        for_window [app_id="Bluetooth-cli"] floating enable
        for_window [app_id="m-connection-editor"] floating enable
        # prevent locking when mpv window is open
        for_window [app_id="mpv"] inhibit_idle visible
      '';
    };
  };
}
