{ config, pkgs, ... }:

{
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = {
        primary = true;
        address = "francis@begyn.be";
        userName = "francis@begyn.be";
        realName = "Francis Begyn";
        passwordCommand = "${pkgs.pass}/bin/pass mail/mail.begyn.be";
        imap.host = "mail.begyn.be";
        smtp.host = "mail.begyn.be";
        notmuch.enable = true;
        signature = {
          showSignature = "append";
          text = ''
            ---
            Met vriendelijke groeten
            Kind regards
            Francis Begyn
          '';
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [
            "INBOX" "Sent" "Drafts" "Trash"
            "Archives" "Junk"
            "hosting"
            "libera" "matrix" "excalidraw"
          ];
        };
        folders = {
          inbox = "INBOX";
          sent = "Sent";
          drafts = "Drafts";
          trash = "Trash";
        };
        neomutt = {
          enable = true;
          extraMailboxes = [
            { mailbox = "Sent"; name = "Sent"; }
            { mailbox = "Drafts"; name = "Drafts"; }
            { mailbox = "Trash"; name = "Trash"; }
            { mailbox = "Archives"; name = "Archive"; }
            { mailbox = "hosting"; name = "hosting"; }
          ];
        };
      };
      gmail = {
        flavor = "gmail.com";
        address = "francis.begyn@gmail.com";
        userName = "francis.begyn@gmail.com";
        realName = "Francis Begyn";
        passwordCommand = "${pkgs.pass}/bin/pass mail/gmail-app";
        imap.host = "imap.gmail.com";
        smtp.host = "smtp.gmail.com";
        signature = {
          showSignature = "append";
          text = ''
            ---
            Met vriendelijke groeten
            Kind regards
            Francis Begyn
          '';
        };
        notmuch.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [
            "Inbox" "[Gmail]/*"
            "Belgium" "Financieel" "Development"
            "Templates"
            "NANOG" "Werk"
          ];
        };
        folders = {
          inbox = "Inbox";
          sent = "[Gmail]/Sent Mail";
          drafts = "[Gmail]/Drafts";
          trash = "[Gmail]/Trash";
        };
        neomutt = {
          enable = true;
          extraMailboxes = [
            { mailbox = "[Gmail]/Sent Mail"; name = "Sent"; }
            { mailbox = "[Gmail]/Drafts"; name = "Drafts"; }
            { mailbox = "[Gmail]/Trash"; name = "Trash"; }
            { mailbox = "[Gmail]/All Mail"; name = "Archive"; }
            { mailbox = "NANOG"; name = "NANOG"; }
          ];
        };
      };
      sko = {
        address = "francis.begyn@studentkickoff.be";
        userName = "francis.begyn@studentkickoff.be";
        realName = "Francis Begyn";
        passwordCommand = "${pkgs.pass}/bin/pass mail/sko-app";
        imap.host = "outlook.office365.com";
        smtp.host = "smtp.office365.com";
        notmuch.enable = true;
        signature = {
          showSignature = "append";
          text = ''
            ---
            Met vriendelijke groeten
            Francis Begyn
            Bestuurder
            https://studentkickoff.be
            +32484365715
          '';
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [
            "Inbox" "Verzonden items" "Concepten" "Verwijderde items"
            "Ongewenste e-mail" "Archief" "Archives"
          ];
        };
        folders = {
          inbox = "Inbox";
          sent = "Verzonden items";
          drafts = "Concepten";
          trash = "Verwijderde items";
        };
        neomutt = {
          enable = true;
          extraMailboxes = [
            { mailbox = "Verzonden items"; name = "Sent"; }
            { mailbox = "Concepten"; name = "Concepten"; }
            { mailbox = "Verwijderde items"; name = "Trash"; }
            { mailbox = "Archief"; name = "Archief"; }
          ];
        };
      };
    };
  };

  # mbsync setup: still needs some work to be fully functional
  home.packages = [
    pkgs.isync
    pkgs.w3m
    pkgs.mu
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
      [Filter.1]
      message = mark ll mail in junk/trash folders as trash
      query = folder:gmail/[Gmail]/Spam
      tags = +trash -inbox -unread
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
      folders = francis@begyn.be/INBOX
        francis@begyn.be/Archive
        francis@begyn.be/Sent
        "francis.begyn@gmail.com/[Gmail]/Sent Mail"
        "francis.begyn@gmail.com/[Gmail]/All Mail"
        francis.begyn@gmail.com/Inbox
        francis.begyn@studentkickoff.be/Inbox
        francis.begyn@studentkickoff.be/Archief
        "francis.begyn@studentkickoff.be/Sent Items"
      # personal
      francis@begyn.be/INBOX = 'NOT tag:inbox':francis@begyn.be/Archive
      francis@begyn.be/Archive = 'tag:trash':francis@begyn.be/Trash
        'tag:inbox':francis@begyn.be/Inbox
        'tag:spam':francis@begyn.be/Spam
      francis@begyn.be/Sent = 'tag:trash':francis@begyn.be/Trash
      # gmail
      francis.begyn@gmail.com/Inbox = 'NOT tag:inbox':"francis.begyn@gmail.com/[Gmail]/All Mail"
      francis.begyn@gmail.com/[Gmail]/All Mail = 'tag:trash':francis.begyn@gmail.com/[Gmail]/Trash
        'tag:inbox':francis.begyn@gmail.com/Inbox
        'tag:spam':francis.begyn@gmail.com/[Gmail]/Spam
      francis.begyn@gmail.com/[Gmail]/Sent Mail = 'tag:trash':francis.begyn@gmail.com/[Gmail]/Trash
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
    tags = {
      todo = {
        translated = "TODO";
        normal = "'','', 'white','light red', 'white','#d66'";
      };
      flagged = {
        translated = "⚑";
        normal = "'','','light red','','light red',''";
        focus = "'','','light red','','light red',''";
      };
      unread = {
        translated = "✉";
      };
      replied = {
        translated = "⏎";
      };
      encrypted = {
        translated = "⚷";
      };
    };
    extraConfig = ''
      attachment_prefix = "~/Downloads"
      editor_spawn = False
      history_size = 200
      terminal_cmd = "alacritty -e"
      timestamp_format = "%Y/%m/%d %H:%M"
      ask_subject = True
      theme = mutt
      thread_focus_linewise = True
    '';
  };
  programs.neomutt = {
    enable = true;
    vimKeys = true;
    sidebar = {
      enable = true;
    };
    extraConfig = ''
      ###############################################################################
      # Dracula Theme for Mutt: https://draculatheme.com/
      #
      # @author Paul Townsend <paul@caprica.org>
      # general ------------ foreground ---- background -----------------------------
      color error		color231	color212
      color indicator		color231	color241
      color markers		color210	default
      color message		default		default
      color normal		default		default
      color prompt		default	        default
      color search		color84		default
      color status 		color141	color236
      color tilde		color231	default
      color tree		color141	default
      # message index ------ foreground ---- background -----------------------------
      color index		color210	default 	~D # deleted messages
      color index		color84		default 	~F # flagged messages
      color index		color117	default 	~N # new messages
      color index		color212	default 	~Q # messages which have been replied to
      color index		color215	default 	~T # tagged messages
      color index		color141	default		~v # messages part of a collapsed thread
      # message headers ---- foreground ---- background -----------------------------
      color hdrdefault	color117	default
      color header		color231	default		^Subject:.*
      # message body ------- foreground ---- background -----------------------------
      color attachment	color228	default
      color body		color231	default		[\-\.+_a-zA-Z0-9]+@[\-\.a-zA-Z0-9]+               # email addresses
      color body		color228	default		(https?|ftp)://[\-\.,/%~_:?&=\#a-zA-Z0-9]+        # URLs
      color body		color231	default		(^|[[:space:]])\\*[^[:space:]]+\\*([[:space:]]|$) # *bold* text
      color body		color231	default		(^|[[:space:]])_[^[:space:]]+_([[:space:]]|$)     # _underlined_ text
      color body		color231	default		(^|[[:space:]])/[^[:space:]]+/([[:space:]]|$)     # /italic/ text
      color quoted		color61		default
      color quoted1		color117	default
      color quoted2		color84		default
      color quoted3		color215	default
      color quoted4		color212	default
      color signature		color212	default
    '';
  };
}
