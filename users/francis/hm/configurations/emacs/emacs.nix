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
      (set-default-font "${font}-${fontSize}")
      (set-face-attribute 'default nil :font "${font}-${fontSize}")
      (set-frame-font "${font}-${fontSize}" nil t))

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
      bindLocal.company-active-map = {
        "M-n" = "nil";
        "M-p" = "nil";
        "C-n" = "#'company-select-next";
        "C-p" = "#'company-select-previous";
      };
      config = ''
        (global-company-mode t)
      '';
      extraConfig = ''
        :custom
        ;; (company-begin-commands nil)
        (company-minimum-prefix-length 0)
        (company-idle-delay 0.3)
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
    dired = {
      enable = true;
      bind = {
        "<f9>" = "mhj/toggle-project-explorer";
      };
      config = ''
        (progn
          (setq dired-listing-switches "-lXGh --group-directories-first")
          (add-hook 'dired-mode-hook 'dired-omit-mode)
          (add-hook 'dired-mode-hook 'dired-hide-details-mode))

        (defun mhj/toggle-project-explorer ()
          "Toggle the project explorer window."
          (interactive)
          (let* ((buffer (dired-noselect (projectile-project-root)))
            (window (get-buffer-window buffer)))
            (if window
            (mhj/hide-project-explorer)
              (mhj/show-project-explorer))))

        (defun mhj/show-project-explorer ()
          "Project dired buffer on the side of the frame.
        Shows the projectile root folder using dired on the left side of
        the frame and makes it a dedicated window for that buffer."
          (let ((buffer (dired-noselect (projectile-project-root))))
            (progn
              (display-buffer-in-side-window buffer '((side . left) (window-width . 0.2)))
              (set-window-dedicated-p (get-buffer-window buffer) t))))

        (defun mhj/hide-project-explorer ()
          "Hide the project-explorer window."
          (let ((buffer (dired-noselect (projectile-project-root))))
            (progn
              (delete-window (get-buffer-window buffer))
              (kill-buffer buffer))))
      '';
    };
    dired-subtree = {
      enable = true;
      bindLocal = {
        dired-mode-map = {
          "<enter>" = "mhj/dwim-toggle-or-open";
          "<return>" = "mhj/dwim-toggle-or-open";
          "<tab>" = "mhj/dwim-toggle-or-open";
          "<down-mouse-1>" = "mhj/mouse-dwim-to-toggle-or-open";
        };
      };
      config = ''
        (progn
          ;; Function to customize the line prefixes (I simply indent the lines a bit)
          (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
          (setq dired-subtree-use-backgrounds nil))

        (defun mhj/dwim-toggle-or-open ()
          "Toggle subtree or open the file."
          (interactive)
          (if (file-directory-p (dired-get-file-for-visit))
              (progn
            (dired-subtree-toggle)
            (revert-buffer))
            (dired-find-file)))

        (defun mhj/mouse-dwim-to-toggle-or-open (event)
          "Toggle subtree or the open file on mouse-click in dired."
          (interactive "e")
          (let* ((window (posn-window (event-end event)))
             (buffer (window-buffer window))
             (pos (posn-point (event-end event))))
            (progn
              (with-current-buffer buffer
            (goto-char pos)
            (mhj/dwim-toggle-or-open)))))
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
      config = ''
        (setq evil-collection-mode-alist (delete 'go-mode evil-collection-mode-list))
        (evil-collection-init)
      '';
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
    evil-args = {
      enable = true;
      # after = ["evil"];
      bindLocal.evil-inner-text-objects-map = {
        "a" = "evil-inner-arg";
      };
      bindLocal.evil-outer-text-objects-map = {
        "a" = "evil-outer-arg";
      };
    };
    evil-commentary = {
      enable = true;
      after = ["evil"];
      config = ''
        (evil-commentary-mode)
      '';
      diminish = ["evil-commentary-mode"];
    };
    evil-fringe-mark = {
      enable = true;
      after = ["evil"];
      config = ''
        (setq-default evil-fringe-mark-show-special t)
        (push ?{ evil-fringe-mark-ignore-chars)
        (push ?} evil-fringe-mark-ignore-chars)
        (global-evil-fringe-mark-mode)
      '';
      diminish = ["global-evil-fringe-mark-mode"];
    };
    evil-indent-plus = {
      enable = true;
      after = ["evil"];
      config = ''
        (evil-indent-plus-default-bindings)
      '';
    };
    evil-matchit = {
      enable = true;
      after = ["evil"];
      config = ''
        (global-evil-matchit-mode 1)
      '';
    };
    evil-exchange = {
      enable = true;
      after = ["evil"];
      config = ''
        (evil-exchange-cx-install)
      '';
    };
    evil-easymotion = {
      enable = true;
      # after = ["evil"];
      bindLocal.evil-motion-state-map = {
        "SPC C-n" = "evilem-motion-next-line";
        "SPC C-p" = "evilem-motion-previous-line";
      };
      config = ''
        (evilem-default-keybindings "SPC")
      '';
    };
    evil-numbers = {
      enable = true;
      bindLocal.evil-normal-state-map = {
        "<kp-add>" = "evil-numbers/inc-at-pt";
        "<kp-subtract>" = "evil-numbers/dec-at-pt";
      };
      extraConfig = ''
        :custom
        (evil-numbers-pad-default t)
      '';
    };
    evil-surround = {
      enable = true;
      after = ["evil"];
      config = ''
        (global-evil-surround-mode 1)
      '';
    };
    evil-goggles = {
      enable = false; # these were learning wheels.
      after = ["evil"];
      diminish = ["evil-goggles-mode"];
      config = ''
        (evil-goggles-mode)
        (evil-goggles-use-diff-faces)
      '';
    };

    expand-region = {
      enable = true;
      bind = {
        "C-=" = "er/expand-region";
      };
    };

    flycheck = {
      enable = true;
      hook = [ "(prog-mode . flycheck-mode)" ];
      diminish = ["flycheck-mode"];
      extraConfig = ''
        :custom
        (flycheck-display-errors-function 'ignore)
        (flycheck-highlighting-mode nil)
        (flycheck-navigation-minimum-level 'error)
        (flycheck-check-syntax-automatically '(save mode-enabled))
        (flycheck-emacs-lisp-load-path 'inherit)
      '';
    };

    cue-mode = {
      enable = true;
    };

    go-mode = {
      enable = true;
      config = ''
        (evil-add-command-properties #'go-goto-arguments :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-docstring :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-function-name :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-method-receiver :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-return-values :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-imports :jump t :repeat 'motion :type 'exclusive)
        (evil-add-command-properties #'go-goto-function :jump t :repeat 'motion :type 'exclusive)
      '';
      hook = [''
        (go-mode . (lambda ()
          (lsp)
          ;; (lsp-semantic-tokens-mode)
          (add-hook 'before-save-hook 'lsp-organize-imports t t)
          (add-hook 'before-save-hook 'lsp-format-buffer t t)
          (subword-mode 1)
          (define-key evil-motion-state-local-map (kbd "gsff") #'go-goto-function)
          (define-key evil-motion-state-local-map (kbd "gsfa") #'go-goto-arguments)
          (define-key evil-motion-state-local-map (kbd "gsfn") #'go-goto-function-name)
          (define-key evil-motion-state-local-map (kbd "gsfr") #'go-goto-method-receiver)
          (define-key evil-motion-state-local-map (kbd "gsfv") #'go-goto-return-values)
          (define-key evil-motion-state-local-map (kbd "gsd")  #'go-goto-docstring)
          (define-key evil-motion-state-local-map (kbd "gsi")  #'go-goto-imports)
        ))
      ''];
      extraConfig = ''
        :custom
        (godoc-at-point-function 'godoc-gogetdoc)
        (gofmt-command "${pkgs.gotools}/bin/goimports")
        (gofmt-show-errors 'buffer)
      '';
    };

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
        (magit-save-repository-buffers 'dontask)
      '';
    };

    lsp-mode = {
      enable = true;
      defer = true;
      config = ''

        ;; (setq lsp-semantic-tokens-enable t)
        ;; (setq lsp-semantic-tokens-honor-refresh-requests t)
        ;; (setq lsp-semantic-tokens-warn-on-missing-face t)

        (lsp-register-custom-settings
          '(("gopls.importShortcut" "Definition" nil)
            ("gopls.staticcheck" nil t)
            ;; ("gopls.semanticTokens" t t)
            ("gopls.experimentalPostfixCompletions" t t)))

        (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
        (lsp-register-client
          (make-lsp-client
            :new-connection (lsp-stdio-connection "/home/dominikh/prj/zls/zig-cache/bin/zls")
            :major-modes '(zig-mode)
            :server-id 'zls))
      '';
      extraConfig = ''
        :custom
        (lsp-signature-render-documentation nil)
        (lsp-file-watch-threshold nil)
        (lsp-auto-execute-action nil)
        (lsp-lens-enable nil)
        (lsp-go-hover-kind "FullDocumentation")
        (lsp-rust-analyzer-server-command '("${pkgs.rust-analyzer}/bin/rust-analyzer"))
        (lsp-rust-server 'rust-analyzer)
      '';
    };
    lsp-ui = {
      enable = true;
      hook = ["(lsp-mode . lsp-ui-mode)"];
      extraConfig = ''
        :custom
        (lsp-ui-doc-enable nil)
        (lsp-ui-peek-enable nil)
        (lsp-ui-sideline-enable nil)
      '';
    };
    lsp-ivy.enable = true;

    lua-mode = {
      enable = true;
      defer = true;
    };

    elixir-mode = {
      enable = true;
      defer = true;
      mode = [''"\\.ex'"''];
    };
    alchemist.enable = true;

    ledger-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.journal\\'"'' ];
      config = ''
        (setq ledger-reconcile-default-commodity "EUR")
      '';
    };

    python-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.py'"'' ];
    };
    virtualenvwrapper.enable = true;

    yaml-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.yaml'"'' ''"\\.yml'"'' ];
    };

    markdown-mode = {
      enable = true;
      defer = true;
      mode = [
        ''("README\\.md\\'" . gfm-mode)''
        ''("\\.md\\'" . markdown-mode)''
        ''("\\.markdown\\'" . markdown-mode)''
      ];
    };

    nix = { enable = true; };

    nix-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.nix\\'"'' ];
      bindLocal = { nix-mode-map = { "C-i" = "nix-indent-line"; }; };
    };

    nix-prettify-mode = {
      enable = true;
      defer = true;
      config = ''
        (nix-prettify-global-mode)
      '';
    };

    nix-drv-mode = {
      enable = true;
      defer = true;
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
      defer = true;
      mode = [ ''"\\.dhall\\'"'' ];
    };

    jsonnet-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.jsonnet\\'"'' ''"\\.libsonnet\\'"'' ];
    };

    rust-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.rs\\'"'' ];
    };

    toml-mode = {
      enable = true;
      defer = true;
      mode = [ ''"\\.toml\\'"'' ];
    };

    nov = {
      enable = true;
      mode = [ ''"\\.epub\\'"'' ];
    };

    web-mode = {
      enable = true;
      defer = true;
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
  };
}

