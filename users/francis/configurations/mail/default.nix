{ config, pkgs, ... }:

{
  home-manager.users.francis = {
    accounts.email = {
      maildirBasePath = ".mail";
      accounts = {
        "francis@begyn.be" = {
          primary = true;
          address = "francis@begyn.be";
          userName = "francis@begyn.be";
          realName = "Francis Begyn";
          passwordCommand = "${pkgs.pass}/bin/pass mailwizard-francis@begyn.be";
          imap.host = "imap.fastmail.com";
          smtp.host = "smtp.fastmail.com";
          notmuch.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = ["Archive" "Drafts" "Inbox" "Sent" "Spam" "Trash"];
          };
        };
        "francis.begyn@gmail.com" = {
          flavor = "gmail.com";
          address = "francis.begyn@gmail.com";
          userName = "francis.begyn@gmail.com";
          realName = "Francis Begyn";
          passwordCommand = "${pkgs.pass}/bin/pass mailwizard-francis.begyn@gmail.com";
          imap.host = "imap.gmail.com";
          smtp.host = "smtp.gmail.com";
          notmuch.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = ["Inbox" "!\"_/*\"" "[Gmail]/*"];
          };
        };
        "francis.begyn@studentkickoff.be" = {
          address = "francis.begyn@studentkickoff.be";
          userName = "francis.begyn@studentkickoff.be";
          realName = "Francis Begyn";
          passwordCommand = "${pkgs.pass}/bin/pass mailwizard-francis.begyn@studentkickoff.be";
          imap.host = "outlook.office365.com";
          smtp.host = "smtp.office365.com";
          notmuch.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = ["Verzonden items" "Verwijderde items" "Inbox" "Archief" "Concepten" "Ongewenste e-mail"];
          };
          folders = {
            drafts = "Concepten";
            trash = "Verwijderde items";
            sent = "Verzonden items";
          };
        };
        "francis.begyn@ugent.be" = {
          address = "francis.begyn@ugent.be";
          userName = "francis.begyn@ugent.be";
          realName = "Francis Begyn";
          passwordCommand = "${pkgs.pass}/bin/pass mailwizard-francis.begyn@ugent.be";
          imap.host = "outlook.office365.com";
          smtp.host = "smtp.office365.com";
          notmuch.enable = true;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = [
              "*"
            ];
          };
          folders = {
            trash = "Deleted Items";
            sent = "Sent Items";
          };
        };
      };
    };

    # mbsync setup: still needs some work to be fully functional
    home.packages = [
      pkgs.isync
      pkgs.w3m
    ];
    programs.mbsync = {
      enable = true;
    };
    services.mbsync = {
      enable = true;
      verbose = true;
    };

    # enable notmuch
    programs.notmuch = {
      enable = true;
      new = {
        tags = ["new" "unread"];
      };
    };
    home.sessionVariables = {
      NOTMUCH_CONFIG = /home/francis/.config/notmuch/notmuchrc;
    };

    # afew setup
    programs.afew = {
      enable = true;
      extraConfig = ''
        # This is the default filter chain
        [SpamFilter]
        [KillThreadsFilter]
        [ListMailsFilter]
        [SentMailsFilter]
        sent_tag = sent
        [ArchiveSentMailsFilter]
        [InboxFilter]
        [InboxFilter.0]
        tags = +unread
        [MailMover]
        rename = True
        folders = francis@begyn.be/Inbox
          francis@begyn.be/Archive
          francis@begyn.be/Sent
          francis.begyn@ugent.be/Inbox
          francis.begyn@ugent.be/Archive
          "francis.begyn@ugent.be/Sent Items"
          "francis.begyn@gmail.com/[Gmail]/Sent Mail"
          "francis.begyn@gmail.com/[Gmail]/All Mail"
          francis.begyn@gmail.com/Inbox
          francis.begyn@studentkickoff.be/Inbox
          francis.begyn@studentkickoff.be/Archief
          "francis.begyn@studentkickoff.be/Sent Items"
        # fastmail
        francis@begyn.be/Inbox = 'NOT tag:inbox':francis@begyn.be/Archive
        francis@begyn.be/Archive = 'tag:trash':francis@begyn.be/Trash
          'tag:inbox':francis@begyn.be/Inbox
          'tag:spam':francis@begyn.be/Spam
        francis@begyn.be/Sent = 'tag:trash':francis@begyn.be/Trash
        # personal mail
        francis.begyn@gmail.com/Inbox = 'NOT tag:inbox':"francis.begyn@gmail.com/[Gmail]/All Mail"
        francis.begyn@gmail.com/[Gmail]/All Mail = 'tag:trash':francis.begyn@gmail.com/[Gmail]/Trash
          'tag:inbox':francis.begyn@gmail.com/Inbox
          'tag:spam':francis.begyn@gmail.com/[Gmail]/Spam
        francis.begyn@gmail.com/[Gmail]/Sent Mail = 'tag:trash':francis.begyn@gmail.com/[Gmail]/Trash
        # ugent
        francis.begyn@ugent.be/Inbox = 'NOT tag:inbox':francis.begyn@ugent.be/Archive
        francis.begyn@ugent.be/Archive = 'tag:trash':"francis.begyn@ugent.be/Deleted Items"
          'tag:inbox':francis.begyn@ugent.be/Inbox
          'tag:spam':"francis.begyn@ugent.be/Junk Email"
        francis.begyn@ugent.be/Sent Items = 'tag:trash':"francis.begyn@ugent.be/Deleted Items"
        #sko
        francis.begyn@studentkickoff.be/Inbox = 'NOT tag:inbox':francis.begyn@studentkickoff.be/Archief
        francis.begyn@studentkickoff.be/Archief = 'tag:trash':"francis.begyn@studentkickoff.be/Verwijderde items"
          'tag:inbox':francis.begyn@studentkickoff.be/Inbox
          'tag:spam':"francis.begyn@studentkickoff.be/Ongewenste e-mail"
        francis.begyn@studentkickoff.be/Sent Items = 'tag:trash':'francis.begyn@studentkickoff.be/Verwijderde Items'
      '';
    };

    # alot setup
    programs.alot = {
      enable = true;
      bindings = {
        global = {
          up ="";
          down ="";
          "mouse press 4" = "move up";
          "mouse press 5" = "move down";
          j = "move down";
          k = "move up";
          J = "move page down";
          K = "move page up";
          "g g" = "move first";
          G = "move last";
          "ctrl j" = "move halfpage down";
          "ctrl k" = "move halfpage up";
          "ctrl r" = "refresh";
          "?" = "help bindings";
          I = "search tag:inbox AND NOT tag:killed";
          "#" = "taglist";
          "shift tab" = "bprevious";
          U = "search tag:unread";
          tab = "bnext";
          " " = "prompt 'search '";
          d = "bclose";
          "$" = "flush";
          m = "compose";
          o = "prompt 'search '";
          q = "exit";
          ";" = "bufferlist";
          ":" = "prompt";
          "." = "repeat";
        };
        bufferlist = {
          x = "close";
          enter = "open";
        };
        search = {
          enter = "select; fold *; unfold tag:unread; move last; unfold";
          a = "toggletags inbox";
          "&" = "toggletags killed";
          "!" = "toggletags flagged";
          s = "toggletags unread";
          t = "toggletags todo";
          d = "toggletags trash";
          l = "retagprompt";
          O = "refineprompt";
          "|" = "refineprompt";
        };
        envelope = {
          a = "prompt 'attach ~/'";
          y = "send";
          P = "save";
          s = "'refine Subject'";
          f = "prompt 'set From '";
          t = "'refine To'";
          b = "'refine Bcc'";
          c = "'refine Cc'";
          S = "togglesign";
          enter = "edit";
          "g f" = "togglesource";
        };
        taglist = {
          enter = "select";
        };
        thread = {
          enter = "select";
          C = "fold *";
          E = "unfold *";
          c = "fold";
          "e" = "unfold";
          "<" = "fold";
          ">" = "unfold";
          "[" = "indent -";
          "]" = "indent +";
          "g f" = "togglesource";
          H = "toggleheaders";
          P = "print --all --separately --add_tags";
          S = "save --all";
          g = "reply --all";
          f = "forward";
          p = "print --add_tags";
          n = "editnew";
          b = "bounce";
          s = "save";
          r = "reply";
          "|" = "prompt 'pipeto '";
          v = "pipeto urlscan 2>/dev/null";
          " " = "fold; untag unread; move next unfolded";
          "g j" = "move next sibling";
          "g k" = "move previous sibling";
          "g h" = "move parent";
          "g l" = "move first reply";
          "ctrl u" = "call hooks.unsubscribe(ui)";
        };
      };
      extraConfig = ''
        attachment_prefix = "~/Downloads"
        auto_remove_unread = True
        editor_spawn = False
        history_size = 200
        terminal_cmd = "alacritty -e"
        timestamp_format = "%Y/%m/%d %H:%M"
        ask_subject = True
        theme = mutt
        thread_focus_linewise = True
        [tags]
          [[todo]]
            normal = "","", 'white','light red', 'white','#d66'
            translated = TODO
          [[flagged]]
            translated = ⚑
            normal = "","",'light red',"",'light red',""
            focus = "","",'light red',"",'light red',""
          [[unread]]
            translated = ✉
          [[replied]]
            translated = ⏎
          [[encrypted]]
            translated = ⚷
          '';
    };
  };
}
