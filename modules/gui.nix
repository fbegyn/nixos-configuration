{ config, lib, ... }

{
  imports = [
    ../common/users/francis/i3
    ../common/users/francis/dunst
    ../common/users/francis/mpv
  ];

  options.services.gui {
    enable = lib.mkOption {
      default = false;
      example = true;
    }
  }

  config = {
    services.redshift = {
      enable = true;
      latitude = "51.05389";
      longitude = "3.705";
    };

    programs.newsboat = {
      enable = true;
      urls = [
        { url = https://lobste.rs/rss; }
        { url = http://sreweekly.com/feed/; }
        { url = https://blog.cloudflare.com/rss/; }
        { url = https://news.ycombinator.com/rss; }
        { url = https://www.vrt.be/vrtnws/nl.rss.headlines.xml; }
        { url = https://www.ugent.be/RSS; }
      ];
      autoReload = true;
      reloadTime = 300;
      extraConfig = ''
        # Marcos
        macro m set browser "/usr//bin/mpv %u"; open-in-browser ; set browser "/usr/bin/qutebrowser %u"
        macro t set browser "/usr/bin/w3m %u"; open-in-browser ; set browser "/usr/bin/qutebrowser %u"
        # unbind defaults
        unbind-key ENTER
        unbind-key h
        unbind-key j
        unbind-key k
        unbind-key l
        # vim style movement
        bind-key h quit
        bind-key j down
        bind-key k up
        bind-key l open
        # home/end
        unbind-key g
        bind-key g home
        unbind-key G
        bind-key G end
      '';
    };

    home.file.".config/compton.conf".source = ./compton.conf;
    home.file.".config/qutebrowser/config.py".source = ./qutebrowser/config.py;

    home.packages = with pkgs; [
      # Comms
      slack
      mattermost-desktop
      teamspeak_client
      # Browser
      firefox
      qutebrowser
      # Utilities
      compton
      nfs-utils
      gnome3.nautilus
      rofi
      rofi-pass
      vscode
    ];

  }
}
