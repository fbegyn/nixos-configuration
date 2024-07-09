{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    package = pkgs.unstable.alacritty;
    settings = {
      live_config_reload = true;
      bell.duration = 0;
      cursor.unfocused_hollow = true;
      env.TERM = "xterm-256color";
      font = {
        size = 10;
        glyph_offset = {
          x = 0;
          y = 0;
        };
        offset = {
          x = 0;
          y = 0;
        };
        normal = {
          family = "DejaVu Sans Mono";
          style = "Regular";
        };
      };
      mouse = {
        hide_when_typing = true;
        bindings = [{
          action = "PasteSelection";
          mouse = "Middle";
        }];
      };
      selection.semantic_escape_chars = ",|`|:\"' ()[]{}<>";
      window = {
        decorations = "full";
        dynamic_title = true;
        opacity = 0.9;
        dimensions = {
          columns = 0;
          lines = 0;
        };
        padding = {
          x = 2;
          y = 2;
        };
      };
      colors = {
        primary = {
          foreground = "#${config.colorScheme.palette.base05}";
          background = "#${config.colorScheme.palette.base00}";
        };
        normal = {
          black = "#1c1b19";
          blue = "#${config.colorScheme.palette.base0D}";
          cyan = "#${config.colorScheme.palette.base0C}";
          green = "#${config.colorScheme.palette.base0B}";
          magenta = "#${config.colorScheme.palette.base0E}";
          red = "#${config.colorScheme.palette.base08}";
          yellow = "#${config.colorScheme.palette.base0A}";
          white = "#d0bfa1";
        };
      };
    };
  };
}
