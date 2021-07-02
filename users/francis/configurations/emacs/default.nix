{ pkgs, ... }:

let e = pkgs.writeTextFile {
      name = "francis-emacs.desktop";
      destination = "/share/applications/francis-emacs.desktop";
      text = ''
[Desktop Entry]
Exec=emacsclient -nc
Icon=emacs
Name[en_US]=Emacs Client
Name=Emacs Client
StartupNotify=true
Terminal=false
Type=Application
      '';
    };
in
{
  imports = [ ./emacs-init.nix ];

  home.packages = [
    pkgs.ispell
    e
  ];

  home.file = {
    ".local/bin/e" = {
      text = ''
        #!/bin/sh
        emacsclient -t -a "" $@
      '';
      executable = true;
    };
    ".local/bin/ew" = {
      text = ''
        #!/bin/sh
        emacsclient -a "" -nc $@
      '';
      executable = true;
    };
  };

  programs.emacs = {
    enable = true;
    init = {
      enable = true;

      recommendedGcSettings = true;

      prelude = let
        fontSize = "15";
        emacsFont = ''
          (when window-system
            (set-frame-font "DejaVu Sans Mono ${fontSize}"))
        '';
      in emacsFont + ''
        (require 'bind-key)

        (setq inhibit-startup-screen t) ; don't show starup screen

        (menu-bar-mode -1) ; we don't need a menu bar

        (electric-pair-mode) ; insert matching delimiters

        (recentf-mode 1) ; builds list of recently opened files
        (setq recentf-max-menu-items 25) ; max 25 items in menu
        (setq recentf-max-saved-items 25) ; max size of recent files list
        (global-set-key "\C-x\ \C-r" 'recentf-open-files)

        (when window-system
          (dolist (mode
            '(tool-bar-mode
              tooltip-mode
              scroll-bar-mode
              menu-bar-mode
              blink-cursor-mode))
            (funcall mode 0)))

        (add-hook 'text-mode-hook 'auto-fill-mode) ; automatically reflow text (M-q)

        (setq delete-old-versions -1 )		  ; delete excess backup versions silently
        (setq version-control t )	     	  ; use version control
        (setq vc-make-backup-files t )		  ; make backups file even when in version controlled dir
        (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
        (setq vc-follow-symlinks t )	      ; don't ask for confirmation when opening symlinked file
        (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name
        (setq inhibit-startup-screen t )      ; inhibit useless and old-school startup screen
        (setq ring-bell-function 'ignore )	  ; silent bell when you make a mistake
        (setq coding-system-for-read 'utf-8 ) ; use utf-8 by default
        (setq coding-system-for-write 'utf-8 ); use utf-8 by default
        (setq sentence-end-double-space nil)  ; sentence SHOULD end with only a point.
        (setq-default fill-column 81)		  ; toggle wrapping text at the 81th character

        (setq initial-scratch-message "coi")  ; print a default message in the empty scratch buffer opened at startup

        ;; line numbers
        (when (version<= "26.0.50" emacs-version )
            (global-display-line-numbers-mode))

        ;; tweak some parameters
        (set-frame-parameter (selected-frame) 'alpha '(100 . 90))
        (add-to-list 'default-frame-alist '(alpha . (100 . 90)))

        ;; some general behavior settings
        (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
        (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
        (setq default-tab-width 2)
        (setq frame-resize-pixelwise t)

        ;; rust
        (setq rust-format-on-save t)

        ;; refresh file with f5
         (global-set-key
           (kbd "<f5>")
           (lambda (&optional force-reverting)
             "Interactive call to revert-buffer. Ignoring the auto-save
          file and not requesting for confirmation. When the current buffer
          is modified, the command refuses to revert it, unless you specify
          the optional argument: force-reverting to true."
             (interactive "P")
             ;;(message "force-reverting value is %s" force-reverting)
             (if (or force-reverting (not (buffer-modified-p)))
                 (revert-buffer :ignore-auto :noconfirm)
               (error "The buffer has been modified"))))

        ;; extra functions for emacs
        (defun chomp (str)
          "Chomp leading and tailing whitespace from STR."
          (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                              str)
            (setq str (replace-match "" t t str)))
          str)

        (defun eshell/e (arg)
          "opens a given file in emacs from eshell"
          (find-file arg))

        (defun eshell/eh (arg)
          "opens a file in emacs from shell horizontally"
          (split-window-vertically)
          (other-window 1)
          (find-file arg))

        (defun eshell/ev (arg)
          "opens a file in emacs from shell vertically"
          (split-window-horizontally)
          (other-window 1)
          (find-file arg))

         ;; update all buffers after a git chance
         (defun revert-all-buffers ()
           "Iterate through the list of buffers and revert them, e.g. after a
            new branch has been checked out."
            (interactive)
            (when (yes-or-no-p "Are you sure - any changes in open buffers will be lost! ")
              (let ((frm1 (selected-frame)))
                (make-frame)
                (let ((frm2 (next-frame frm1)))
                  (select-frame frm2)
                  (make-frame-invisible)
                  (dolist (x (buffer-list))
                    (let ((test-buffer (buffer-name x)))
                      (when (not (string-match "\*" test-buffer))
                        (when (not (file-exists-p (buffer-file-name x)))
                          (select-frame frm1)
                          (when (yes-or-no-p (concat "File no longer exists (" (buffer-name x) "). Close buffer? "))
                            (kill-buffer (buffer-name x)))
                          (select-frame frm2))
                        (when (file-exists-p (buffer-file-name x))
                          (switch-to-buffer (buffer-name x))
                          (revert-buffer t t t)))))
                  (select-frame frm1)
                  (delete-frame frm2)))))
      '';

      usePackageVerbose = true;
      usePackage = {
        # Company text completion mode - complete anything
        company = {
          enable = true;
          diminish = [ "company-mode" ];
          config = ''
            (company-mode)
            (setq company-idle-delay 0)
            (setq company-minium-prefix-lenght 1)
          '';
        };

        # sidebar file explorer
        neotree = {
          enable = true;
          config = ''
            (global-set-key [f8] 'neotree-toggle)
            (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
            (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
            (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
            (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
            (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
            (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
            (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
            (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
            (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
          '';
        };
        all-the-icons = {
          enable = true;
          config = ''
            (setq neo-theme 'icons)
          '';
        };

        # improved common emacs commands
        counsel = {
          enable = true;
          bindStar = {
            "M-x" = "counsel-M-x";
            "C-x C-f" = "counsel-find-file";
            "C-x C-r" = "counsel-recentf";
            "C-c f" = "counsel-git";
            "C-c s" = "counsel-git-grep";
            "C-c /" = "counsel-rg";
            "C-c l" = "counsel-locate";
            "M-y" = "counsel-yank-pop";
          };
          general = ''
            (general-nmap
              :prefix "SPC"
              "SPC" '(counsel-M-x :which-key "M-x")
              "ff"  '(counsel-find-file :which-key "find file")
              "s"   '(:ignore t :which-key "search")
              "sc"  '(counsel-unicode-char :which-key "find character"))
          '';
        };

        cython-mode = { enable = true; };

        # Direnv intergration for emacs
        direnv = {
          enable = true;
          config = ''
            (direnv-mode)
          '';
        };

        better-defaults = { enable = true; };

        # Vi bindings for emacs
        evil = {
          enable = true;
          init = ''
            (setq evil-want-C-i-jump nil)
          '';
          config = ''
            (evil-mode 1)
          '';
        };
        evil-surround = {
          enable = true;
          config = ''
            (global-evil-surround-mode 1)
          '';
        };
        evil-collection = {
          enable = true;
          after = [ "evil" ];
        };
        evil-magit = {
          enable = true;
          after = [ "magit" ];
        };

        yasnippet = { enable = true; };

        # syntax checking for emacs
        flycheck = {
          enable = true;
          diminish = [ "flycheck-mode" ];
          config = ''
            (global-flycheck-mode)
          '';
        };

        go-mode = {
          enable = true;
          config = ''
            (setenv "GOPATH" (concat (getenv "HOME") "/go"))
            (setenv "PATH" (concat
                (getenv "PATH")
                ":"
                (getenv "GOPATH") "/bin"
            ))
            (setq gofmt-command "goimports")
            (add-hook 'go-mode-hook #'lsp-deferred)
            (add-hook 'go-mode-hook #'yas-minor-mode)
         '';
        };

        lsp-mode = {
          enable = true;
          command = [ "lsp" ];
          hook = [
            "(go-mode . lsp)"
            "(rust-mode . lsp)"
            "(lsp-mode . lsp-enable-which-key-integration)"
          ];
          config = ''
            (setq lsp-rust-server 'rust-analyzer)
          '';
        };
        lsp-ui = {
          enable = true;
          after = [ "lsp" ];
          command = [ "lsp-ui-mode" ];
        };
        lsp-ivy = {
          enable = true;
          after = [ "lsp" "ivy" ];
          command = [ "lsp-ivy-workspace-symbol" ];
        };

        # some general settings
        general = {
          enable = true;
          after = [ "evil" "which-key" ];
          config = ''
            (general-evil-setup)

            (general-mmap
              ":" 'evil-ex
              ";" 'evil-repeat-find-char)

            (general-create-definer my-leader-def
              :prefix "SPC")

            (general-create-definer my-local-leader-def
              :prefix "SPC m")

            (general-nmap
              :prefix "SPC"
              "b"  '(:ignore t :which-key "buffer")
              "bd" '(kill-this-buffer :which-key "kill buffer")

              "f"  '(:ignore t :which-key "file")
              "ff" '(find-file :which-key "find")
              "fs" '(save-buffer :which-key "save")
              "ft" '(neotree-toggle :which-key "neotree")

              "m"  '(:ignore t :which-key "mode")

              "t"  '(:ignore t :which-key "toggle")
              "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
              "wv" '(split-window-horizontally :which-key "split vertical")
              "ws" '(split-window-vertically :which-key "split horizontal")
              "wk" '(evil-window-up :which-key "up")
              "wj" '(evil-window-down :which-key "down")
              "wh" '(evil-window-left :which-key "left")
              "wl" '(evil-window-right :which-key "right")
              "wd" '(delete-window :which-key "delete")

              "q"  '(:ignore t :which-key "quit")
              "qq" '(save-buffers-kill-emacs :which-key "quit"))
          '';
        };

        ivy = {
          enable = true;
          demand = true;
          diminish = [ "ivy-mode" ];
          config = ''
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t
                  ivy-hight 20
                  ivy-count-format "(%d/%d) "
                  ivy-initial-inputs-alist nil)
          '';
          general = ''
            (general-nmap
              :prefix "SPC"
              "bb" '(ivy-switch-buffer :which-key "switch buffer")
              "fr" '(ivy-recentf :which-key "recent file"))
          '';
        };

        magit = {
          enable = true;
          general = ''
            (general-nmap
              :prefix "SPC"
              "g" '(:ignore t :which-key "Git")
              "gs" 'magit-status)
          '';
        };

        nix = { enable = true; };
        nix-mode = {
          enable = true;
          mode = [ ''"\\.nix\\'"'' ];
          bindLocal = { nix-mode-map = { "C-i" = "nix-indent-line"; }; };
        };
        nix-prettify-mode = {
          enable = true;
          config = ''
            (nix-prettify-global-mode)
          '';
        };
        nix-drv-mode = {
          enable = true;
          mode = [ ''"\\.drv\\'"'' ];
        };

        projectile = {
          enable = true;
          after = [ "ivy" ];
          diminish = [ "projectile-mode" ];
          config = ''
            (projectile-mode 1)
            (progn
              (setq projectile-enable-caching t)
              (setq projectile-require-project-root nil)
              (setq projectile-completion-system 'ivy)
              (add-to-list 'projectile-globally-ignored-files ".DS_Store"))
          '';
          general = ''
            (general-nmap
              :prefix "SPC"
              "p"  '(:ignore t :which-key "Project")
              "pf" '(projectile-find-file :which-key "Find in project")
              "pl" '(projectile-switch-project :which-key "Switch project"))
          '';
        };

        swiper = {
          enable = true;
          bindStar = { "C-s" = "swiper"; };
          general = ''
            (general-nmap
              :prefix "SPC"
              "ss" '(swiper :which-key "swiper"))
          '';
        };

        # hints for shortcuts and bindings
        which-key = {
          enable = true;
          diminish = [ "which-key-mode" ];
          config = ''
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)
            (setq which-key-sort-order 'which-key-key-order-alpha
                  which-key-side-window-max-width 0.33
                  which-key-idle-delay 0.05)
          '';
        };

        gruvbox-theme = {
          enable = true;
          config = ''
            (setq custom-safe-themes t)
            (add-hook 'after-init-hook (lambda () (load-theme 'gruvbox t)))
          '';
        };

        dhall-mode = {
          enable = true;
          mode = [ ''"\\.dhall\\'"'' ];
        };

        elixir-mode = {
          enable = true;
          mode = [''"\\.ex'"''];
        };
        alchemist.enable = true;

        ledger-mode = {
          enable = true;
          mode = [''"\\.journal\\'"''];
          config = ''
            (setq ledger-reconcile-default-commodity "EUR")
          '';
        };

        markdown-mode = {
          enable = true;
          command = [ "markdown-mode" "gfm-mode" ];
          mode = [
            ''("README\\.md\\'" . gfm-mode)''
            ''("\\.md\\'" . markdown-mode)''
            ''("\\.markdown\\'" . markdown-mode)''
          ];
        };

        python-mode = {
          enable = true;
          mode = [''"\\.py'"''];
        };

        protobuf-mode = { enable = true; };

        puppet-mode = {
          enable = true;
        };

        rust-mode = {
          enable = true;
          mode = [ ''"\\.rs\\'"'' ];
        };

        toml-mode = {
          enable = true;
          mode = [ ''"\\.toml\\'"'' ];
        };

        web-mode = {
          enable = true;
          mode = [ ''"\\.html\\'"'' ''"\\.tmpl\\'"'' ];
        };

        yaml-mode = {
          enable = true;
          mode = [
            ''"\\.yml\\'"''
            ''"\\.yaml\\'"''
          ];
        };

        weechat = {
          enable = true;
          config = ''
            (setq weechat-mode-default "plain") ;
            (setq weechat-host-default "100.93.146.4") ;
            (setq weechat-port-default 9000) ;
            (setq weechat-inital-lines 250) ;
            (setq weechat-more-lines-amount 30) ;
          '';
          init = ''
            (defvar weechat-formatting-regex
              (rx-let ((attr (in "*!/_|"))   ;NOTE:  is not documented
                     (std  (= 2 digit))
                     (astd (seq attr (= 2 digit)))
                     (ext  (seq "@" (= 5 digit)))
                     (aext (seq "@" attr (= 5 digit))))
                (rx
                 (or (seq ""
                           (or std
                               ext
                               (seq "F" (or std astd ext aext))
                               (seq "B" (or std ext))
                               (seq "*" (or std
                                            astd
                                            ext
                                            aext
                                            (seq (or std astd ext aext)
                                                 ","
                                                 (or std astd ext aext))))
                               (seq "b" (in "-FDB#_il"))
                               ""))
                      (seq "" attr)
                      (seq "" attr)
                      ""))))
          '';
        };

        virtualenvwrapper.enable = true;

        docker.enable = true;
        docker-tramp.enable = true;
        dockerfile-mode.enable = true;

        ob.enable = true;

        org-download.enable = true;
        org.enable = true;
        org-mime.enable = true;
        org-pomodoro.enable = true;
        org-projectile.enable = true;

        systemd.enable = true;

        highlight-indent-guides = {
          enable = true;
          config = ''
            (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
          '';
        };
      };
    };
  };
}
