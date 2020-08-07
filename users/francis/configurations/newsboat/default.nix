{ config, pkgs, ... }:

{
  home-manager.users.francis.programs.newsboat = {
    enable = true;
    urls = [
      { url = https://lobste.rs/rss; }
      { url = http://sreweekly.com/feed/; }
      { url = https://blog.cloudflare.com/rss/; }
      { url = https://news.ycombinator.com/rss; }
      { url = https://www.vrt.be/vrtnws/nl.rss.headlines.xml; }
      { url = https://www.ugent.be/RSS; }
      { url = https://francis.begyn.be/blog.rss; }
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
}
