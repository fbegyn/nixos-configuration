{ pkgs, config, ... }:

{
  programs.qutebrowser = {
    enable = true;
    package = pkgs.unstable.qutebrowser;
    aliases = {
      w = "session-save";
      q = "quit";
      wq = "quit --save";
      private = "open -p";
    };
    settings = {
      auto_save.session = true;
      content = {
        autoplay = false;
        prefers_reduced_motion = true;
        headers.user_agent = "Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {qt_key}/{qt_version} {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}";
        tls.certificate_errors = "ask-block-thirdparty";
      };
      downloads = {
        location.directory = "/home/francis/Downloads";
        remove_finished = 60000;
      };
      scrolling.smooth = true;
      tabs = {
        background = true;
        new_position.related = "next";
        new_position.stacking = true;
        position = "left";
        title.alignment = "left";
        show = "multiple";
        width = "6%";
      };
      colors = {
        webpage.bg = "";
        webpage.preferred_color_scheme = "auto";
      };
      fonts = {
        hints = "Terminus";
        prompts = "Terminus";
        statusbar = "Terminus";
      };
    };
    searchEngines = {
       DEFAULT = "https://duckduckgo.com/?q={}";
       aw = "https://wiki.archlinux.org/?search={}";
       gscholar = "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q={}";
       g = "https://google.com/search?q={}";
       gif = "https://giphy.com/search/{}";
       gmaps = "https://www.google.be/maps/search/{}+";
       maps = "https://www.openstreetmap.org/search?query={}";
       mov = "https://www.imdb.com/find?q={}&s=all";
       r = "https://www.reddit.com/search?q={}";
       word = "https://www.wordnik.com/words/{}";
       woord = "https://woordenlijst.org/#/?q={}";
    };
    keyBindings = {
      normal = let
        scriptPath = "${pkgs.unstable.qutebrowser}/share/qutebrowser/userscripts";
       in {
         ",c" = "spawn --userscript ${scriptPath}/cast";
      };
      command = {
        "<Ctrl+j>" = "completion-item-focus next";
        "<Ctrl+k>" = "completion-item-focus previous";
      };
    };
  };
}
