{ pkgs }:

let
  lib = pkgs.lib;
  loadImmediately = (name: value: if value ? after then value else value // { demand = true; defer = false; } );
  fontSize = "16";
  font = "DejaVu Sans Mono";
in {
  enable = true;
  recommendedGcSettings = true;
  prelude = ''
    ;; (setq debug-on-error t)
    (when window-system
      (set-frame-font "${font} ${fontSize}"))

    (eval-when-compile (setq use-package-compute-statistics t))

    (require 'use-package)
    (package-initialize)

    (setq inhibit-startup-screen t )      ; inhibit useless and old-school startup screen
    (menu-bar-mode 0) ; we don't need a menu bar

    (load "${./defun.el}")
  '';
  postlude = ''
    (recentf-mode 1)
    (winner-mode 1)

    (setq inhibit-startup-screen t )      ; inhibit useless and old-school startup screen

    (add-hook 'text-mode-hook 'auto-fill-mode) ; automatically reflow text (M-q)

    (setq delete-old-versions -1 )		  ; delete excess backup versions silently
    (setq version-control t )	     	  ; use version control
    (setq vc-make-backup-files t )		  ; make backups file even when in version controlled dir
    (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
    (setq vc-follow-symlinks t )	      ; don't ask for confirmation when opening symlinked file
    (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name
    (setq ring-bell-function 'ignore )	  ; silent bell when you make a mistake
    (setq coding-system-for-read 'utf-8 ) ; use utf-8 by default
    (setq coding-system-for-write 'utf-8 ); use utf-8 by default
    (setq sentence-end-double-space nil)  ; sentence SHOULD end with only a point.
    (setq-default fill-column 81)		  ; toggle wrapping text at the 81th character
    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    (setq initial-scratch-message "coi")  ; print a default message in the empty scratch buffer opened at startup

    ;; line numbers
    (when (version<= "26.0.50" emacs-version )
        (global-display-line-numbers-mode))

    ;; tweak some parameters
    (set-frame-parameter (selected-frame) 'alpha '(100 . 90))
    (add-to-list 'default-frame-alist '(alpha . (100 . 90)))

    ;; some general behavior settings
    (column-number-mode) ; enable column number display
    (setq-default show-trailing-whitespace t)
    (setq-default indicate-empty-lines t) ; check empty lines at the end of file
    (setq-default indicate-buffer-boundaries 'left) ; check for termination on newline
    (setq sentence-end-double-space nil) ; start sentences with a . and single space
    (setq show-paren-delay 0)
    (show-paren-mode)

    (electric-pair-mode) ; insert matching delimiters
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

    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))

    (add-hook 'project-find-functions #'project-find-go-module)
  '';

  usePackageVerbose = false;
  usePackage = lib.mapAttrs loadImmediately {
    company = {
      enable = true;
      diminish = [ "company-mode" ];
      config = ''
        (global-company-mode t)
      '';
      extraConfig = ''
        :custom
        (company-begin-commands nil)
        (company-minimum-prefix-length 0)
        (company-idle-delay 0.0)
        (comapny-tooltip-align-annotations t)
        (company-tooltip-limit 20)
        (company-transformers '(company-sort-by-backend-importance))
      '';
    };
    company-quickhelp = {
      # We don't use company-quickhelp because it uses pos-tip (and
      # thus Emacs tooltips), which cannot be customized as desired.
      # Setting colors works (with limitations), but font size, weight
      # and so on cannot be controlled.
      enable = false;
    };
    company-box = {
      enable = true;
      hook = [ "(company-mode . company-box-mode)" ];
      diminish = ["company-box-mode"];
      config = ''
        (push '(foreground-color . "black") company-box-frame-parameters)
      '';
    };

    dockerfile-mode = { enable = true; };
    docker-tramp = { enable = true; };

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
        (setq-default neo-theme 'icons)
      '';
    };

    highlight-indent-guides = {
      enable = true;
      config = ''
        (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
      '';
    };

    counsel = {
      enable = true;
      demand = true;
      diminish = [ "counsel-mode" ];
      bind = {
        "C-x b" = "counsel-switch-buffer";
      };
      config = ''
        (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
        (setq counsel-switch-buffer-preview-virtual-buffers nil)
        (setq ivy-extra-directories '("./"))
        (counsel-mode 1)
      '';
    };

    better-defaults.enable = true;

    evil-collection = {
      enable = true;
      after = [ "evil" ];
    };
    evil = {
      enable = true;
      demand = true;
      config = ''
        (evil-mode 1)
      '';
      diminish = ["evil-collection-unimpaired-mode"];
      extraConfig = ''
        :custom
        (evil-want-keybinding nil)
        (evil-want-integration t)
        (evil-cross-lines t)
      '';
    };
    evil-commentary = {
      enable = true;
      after = ["evil"];
      config = ''
        (evil-commentary-mode)
      '';
      diminish = ["evil-commentary-mode"];
    };
    evil-indent-plus = {
      enable = true;
      after = ["evil"];
      config = ''
        (evil-indent-plus-default-bindings)
      '';
    };
    evil-surround = {
      enable = true;
      config = ''
        (global-evil-surround-mode 1)
      '';
      after = [ "evil" ];
    };

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
        (setq gofmt-command "goimports")
        (add-hook 'before-save-hook 'gofmt-before-save)
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
        (setq lsp-idle-delay 0.500)
        (setq lsp-modeline-workspace-status-enable nil)
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
    # eglot = {
    #   enable = true;
    #   config = ''
    #     (add-hook 'go-mode-hook 'eglot-ensure)
    #   '';
    # };

    ivy = {
      enable = true;
      demand = true;
      diminish = [ "ivy-mode" ];
      config = ''
        (ivy-mode 1)
      '';
      extraConfig = ''
        :custom
        (ivy-use-virtual-buffers t)
        (ivy-wrap t)
        (ivy-height 20)
        (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
        (ivy-virtual-abbreviate 'full)
      '';
    };
    ivy-hydra.enable = true;
    ivy-rich = {
      enable = false;           # it's too slow at the moment
      demand = true;
      after = ["ivy"];
      config = ''
        (ivy-rich-mode 1)
      '';
    };

    magit = {
      enable = true;
      bind = {
        "<f11>" = "magit-status";
      };
      config = ''
        (magit-auto-revert-mode)
        (global-git-commit-mode 1)
      '';
      extraConfig = ''
        (magit-auto-revert-mode nil)
        (magit-delete-by-moving-to-trash nil)
        (magit-process-popup-time 5)
        (magit-save-repository-buffers 'dontask)
      '';
    };

    elixir-mode = {
      enable = true;
      mode = [''"\\.ex'"''];
    };
    alchemist.enable = true;

    ledger-mode = {
      enable = true;
      mode = [ ''"\\.journal\\'"'' ];
      config = ''
        (setq ledger-reconcile-default-commodity "EUR")
      '';
    };

    python-mode = {
      enable = true;
      mode = [ ''"\\.py'"'' ];
    };
    virtualenvwrapper.enable = true;

    yaml-mode = {
      enable = true;
      mode = [ ''"\\.yaml'"'' ''"\\.yml'"'' ];
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
      diminish = [ "projectile-mode" ];
      config = ''
        (projectile-mode +1)
      '';
    };

    protobuf-mode = { enable = true; };

    swiper = {
      enable = true;
    };

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

    jsonnet-mode = {
      enable = true;
      mode = [ ''"\\.jsonnet\\'"'' ''"\\.libsonnet\\'"'' ];
    };

    rust-mode = {
      enable = true;
      mode = [ ''"\\.rs\\'"'' ];
    };

    toml-mode = {
      enable = true;
      mode = [ ''"\\.toml\\'"'' ];
    };

    nov = {
      enable = true;
      mode = [ ''"\\.epub\\'"'' ];
    };

    web-mode = {
      enable = true;
      mode = [ ''"\\.html\\'"'' ''"\\.tmpl\\'"'' ];
    };

    ob.enable = true;
    org-download.enable = true;
    org.enable = true;
    org-mime.enable = true;
    org-pomodoro.enable = true;
    org-projectile.enable = true;
    systemd.enable = true;
    terraform-mode.enable = true;

    yasnippet = {
      enable = true;
      demand = true;
      config = ''
        (yas-global-mode 1)
      '';
      bind = {
        "<tab>" = "nil";
        "TAB" = "nil";
        "M-TAB" = "yas-expand";
      };
    };

    general = {
      enable = true;
      after = [ "evil" ];
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
        (general-nmap
          :prefix "SPC"
          "bb" '(ivy-switch-buffer :which-key "switch buffer")
          "fr" '(ivy-recentf :which-key "recent file"))
        (general-nmap
          :prefix "SPC"
          "g" '(:ignore t :which-key "Git")
          "gs" 'magit-status)
        (general-nmap
          :prefix "SPC"
          "p"  '(:ignore t :which-key "Project")
          "pf" '(projectile-find-file :which-key "Find in project")
          "pl" '(projectile-switch-project :which-key "Switch project"))
        (general-nmap
          :prefix "SPC"
          "SPC" '(counsel-M-x :which-key "M-x")
          "ff"  '(counsel-find-file :which-key "find file")
          "s"   '(:ignore t :which-key "search")
          "sc"  '(counsel-unicode-char :which-key "find character"))
      '';
    };
  };
}

