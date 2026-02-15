;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Packages are managed by Nix. use-package is for configuration only.

;;; Code:

;; ──────────────────────────────────────────────────────────────
;;  General settings
;; ──────────────────────────────────────────────────────────────

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(dolist (hook '(vterm-mode-hook treemacs-mode-hook org-agenda-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(setq-default fill-column 101)
(global-display-fill-column-indicator-mode 1)

(setq-default truncate-lines t)

(setq-default tab-width 2
              indent-tabs-mode nil)

(setq scroll-margin 8
      hscroll-margin 8
      scroll-conservatively 101)

(global-hl-line-mode 1)
(blink-cursor-mode -1)
(setq use-short-answers t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq select-enable-clipboard t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq require-final-newline t)

(setq-default show-trailing-whitespace t)
(dolist (hook '(minibuffer-setup-hook special-mode-hook vterm-mode-hook
               treemacs-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (setq-local show-trailing-whitespace nil))))

(show-paren-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(savehist-mode 1)
(electric-pair-mode 1)

;; ──────────────────────────────────────────────────────────────
;;  Evil
;; ──────────────────────────────────────────────────────────────

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-undo-system 'undo-redo
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<escape>") #'evil-ex-nohighlight)

  ;; Window (SPC w)
  (evil-define-key 'normal 'global
    (kbd "<leader>wv") #'evil-window-vsplit
    (kbd "<leader>ws") #'evil-window-split
    (kbd "<leader>wd") #'evil-window-delete
    (kbd "<leader>wh") #'evil-window-left
    (kbd "<leader>wj") #'evil-window-down
    (kbd "<leader>wk") #'evil-window-up
    (kbd "<leader>wl") #'evil-window-right
    (kbd "<leader>w=") #'balance-windows)

  ;; Buffer (SPC b)
  (evil-define-key 'normal 'global
    (kbd "<leader>bd") #'kill-current-buffer
    (kbd "<leader>bn") #'next-buffer
    (kbd "<leader>bp") #'previous-buffer)

  ;; File (SPC f)
  (evil-define-key 'normal 'global
    (kbd "<leader>fs") #'save-buffer)

  ;; Quit (SPC q)
  (evil-define-key 'normal 'global
    (kbd "<leader>qq") #'save-buffers-kill-terminal
    (kbd "<leader>qw") #'save-buffers-kill-emacs)

  ;; Center after half-page scroll
  (advice-add #'evil-scroll-down :after (lambda (&rest _) (recenter)))
  (advice-add #'evil-scroll-up :after (lambda (&rest _) (recenter))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

;; ──────────────────────────────────────────────────────────────
;;  Theme & UI
;; ──────────────────────────────────────────────────────────────

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 25
        doom-modeline-buffer-encoding t))

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.25))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))
;;  (setq highlight-indent-guides-method 'character
;;        highlight-indent-guides-character ?\│
;;        highlight-indent-guides-responsive 'top))

;; ──────────────────────────────────────────────────────────────
;;  Minibuffer: Vertico + Orderless + Marginalia + Consult
;; ──────────────────────────────────────────────────────────────

(use-package vertico
  :config
  (vertico-mode 1)
  (setq vertico-count 10
        vertico-cycle t))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package consult
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader><leader>") #'execute-extended-command
    (kbd "<leader>ff")       #'find-file
    (kbd "<leader>fr")       #'consult-recent-file
    (kbd "<leader>bb")       #'consult-buffer
    (kbd "<leader>sr")       #'consult-ripgrep
    (kbd "<leader>ss")       #'consult-line
    (kbd "<leader>sd")       #'consult-flymake
    (kbd "<leader>sh")       #'describe-symbol
    (kbd "<leader>pf")       #'project-find-file
    (kbd "<leader>pg")       #'consult-ripgrep
    (kbd "<leader>pp")       #'project-switch-project))

;; ──────────────────────────────────────────────────────────────
;;  Completion: Corfu + Cape + Dabbrev
;; ──────────────────────────────────────────────────────────────

(use-package corfu
  :config
  (global-corfu-mode 1)
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 10
        corfu-quit-no-match 'separator)
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("RET" . corfu-insert)))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ──────────────────────────────────────────────────────────────
;;  LSP: Eglot
;; ──────────────────────────────────────────────────────────────

(defvar my/nil-auto-eval nil
  "Whether nil_ls autoEvalInputs is enabled.")

(defun my/eglot-workspace-config (_server)
  "Return workspace configuration for eglot servers."
  (list :gopls (list :staticcheck t :analyses (list :fillstruct t))
        :nil (list :nix (list :flake (list :autoEvalInputs
                                           (if my/nil-auto-eval t :json-false))))))

(use-package eglot
  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)

  (setq-default eglot-workspace-configuration #'my/eglot-workspace-config)

  ;; Server programs
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '((elixir-mode elixir-ts-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("deno" "lsp")))

  ;; LSP keybindings
  (evil-define-key 'normal eglot-mode-map
    (kbd "gd")         #'xref-find-definitions
    (kbd "gD")         #'eglot-find-declaration
    (kbd "gr")         #'xref-find-references
    (kbd "gi")         #'eglot-find-implementation
    (kbd "K")          #'eldoc-doc-buffer
    (kbd "<leader>ca") #'eglot-code-actions
    (kbd "<leader>cr") #'eglot-rename
    (kbd "<leader>cf") #'eglot-format-buffer)

  ;; Diagnostics (flymake)
  (evil-define-key 'normal 'global
    (kbd "[d") #'flymake-goto-prev-error
    (kbd "]d") #'flymake-goto-next-error
    (kbd "<leader>cd") #'flymake-show-buffer-diagnostics
    (kbd "<leader>cq") #'flymake-show-project-diagnostics))

;; Start eglot for these modes
(dolist (hook '(go-mode-hook go-ts-mode-hook
               rust-mode-hook rust-ts-mode-hook
               python-mode-hook python-ts-mode-hook
               nix-mode-hook
               elixir-mode-hook elixir-ts-mode-hook
               sh-mode-hook bash-ts-mode-hook
               lua-mode-hook
               js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

;; Format on save helpers

(defun my/eglot-format-on-save ()
  "Format buffer via eglot before saving."
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(defun my/eglot-organize-imports ()
  "Organize imports via eglot code actions."
  (when (eglot-managed-p)
    (ignore-errors
      (eglot-code-action-organize-imports (point-min) (point-max)))))

;; Go: organize imports + format
(dolist (hook '(go-mode-hook go-ts-mode-hook))
  (add-hook hook (lambda ()
                   (add-hook 'before-save-hook #'my/eglot-organize-imports -10 t)
                   (add-hook 'before-save-hook #'my/eglot-format-on-save nil t))))

;; Rust, Elixir, Python: format on save
;; NOTE: Python formatting relies on pyright; for ruff formatting,
;; consider adding apheleia or reformatter with ruff as an external formatter.
(dolist (hook '(rust-mode-hook rust-ts-mode-hook
               elixir-mode-hook elixir-ts-mode-hook
               python-mode-hook python-ts-mode-hook))
  (add-hook hook (lambda ()
                   (add-hook 'before-save-hook #'my/eglot-format-on-save nil t))))

;; Toggle nil autoEvalInputs
(defun my/toggle-nil-auto-eval ()
  "Toggle nil autoEvalInputs and notify the LSP server."
  (interactive)
  (setq my/nil-auto-eval (not my/nil-auto-eval))
  (when-let* ((server (eglot-current-server)))
    (eglot-signal-didChangeConfiguration server))
  (message "nil autoEvalInputs: %s" my/nil-auto-eval))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>cn") #'my/toggle-nil-auto-eval))

;; ──────────────────────────────────────────────────────────────
;;  Tree-sitter
;; ──────────────────────────────────────────────────────────────

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ──────────────────────────────────────────────────────────────
;;  Language modes
;; ──────────────────────────────────────────────────────────────

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.heex\\'"))

(use-package typescript-mode
  :mode "\\.ts\\'")

;; ──────────────────────────────────────────────────────────────
;;  VCS: Magit + diff-hl
;; ──────────────────────────────────────────────────────────────

(use-package magit
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>gg") #'magit-status
    (kbd "<leader>gl") #'magit-log-current
    (kbd "<leader>gb") #'magit-blame))

(use-package diff-hl
  :after evil
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (evil-define-key 'normal 'global
    (kbd "]h") #'diff-hl-next-hunk
    (kbd "[h") #'diff-hl-previous-hunk
    (kbd "<leader>gp") #'diff-hl-show-hunk
    (kbd "<leader>gs") #'diff-hl-stage-current-hunk
    (kbd "<leader>gR") #'diff-hl-revert-hunk
    (kbd "<leader>gr") #'revert-buffer-quick))

(use-package vc-jj
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>js") #'vc-dir
    (kbd "<leader>jl") #'vc-print-log
    (kbd "<leader>jd") #'vc-diff
    (kbd "<leader>ja") #'vc-annotate))

(use-package majutsu
  :ensure nil
  :after evil
  :commands (majutsu majutsu-status majutsu-log)
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>jj") #'majutsu
    (kbd "<leader>jL") #'majutsu-log))

;; ──────────────────────────────────────────────────────────────
;;  TRAMP
;; ──────────────────────────────────────────────────────────────

(require 'tramp)
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^\\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/home/francis/.nix-profile/bin")
(add-to-list 'tramp-remote-path "/etc/profiles/per-user/francis/bin")
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(eval-after-load 'tramp-sh '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(add-to-list 'tramp-connection-properties
             (list ".*" "locale" "LC_ALL=C"))

(tramp-set-completion-function
 "ssh" (append (tramp-get-completion-function "ssh")
               (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                       (directory-files
                        "~/.ssh/conf.d/"
                        'full directory-files-no-dot-files-regexp))))

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024)
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(defun my/tramp-open-remote-file ()
  "Open a file on a remote host via SSH."
  (interactive)
  (let ((host (read-string "SSH host: ")))
    (find-file (concat "/ssh:" host ":~/"))))

(defun my/tramp-open-remote-dir ()
  "Open a directory on a remote host via SSH in dired."
  (interactive)
  (let ((host (read-string "SSH host: ")))
    (dired (concat "/ssh:" host ":~/"))))

(defun my/tramp-open-remote-sudo-dir ()
  "Open a directory on a remote host as sudo via SSH in dired."
  (interactive)
  (let ((host (read-string "SSH host: ")))
    (dired (concat "/ssh:" host "|sudo:" host ":/"))))

(defun my/tramp-open-recent ()
  "Open a recent TRAMP connection."
  (interactive)
  (let* ((recent-files (seq-filter
                        (lambda (f) (file-remote-p f))
                        recentf-list))
         (selected (completing-read "Recent remote: " recent-files nil t)))
    (find-file selected)))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>rf") #'my/tramp-open-remote-file
    (kbd "<leader>rd") #'my/tramp-open-remote-dir
    (kbd "<leader>rs") #'my/tramp-open-remote-sudo-dir
    (kbd "<leader>rr") #'my/tramp-open-recent
    (kbd "<leader>rc") #'tramp-cleanup-all-connections
    (kbd "<leader>rC") #'tramp-cleanup-connection))

;; ──────────────────────────────────────────────────────────────
;;  File tree: Treemacs
;; ──────────────────────────────────────────────────────────────

(use-package treemacs
  :after evil
  :config
  (setq treemacs-width 35
        treemacs-show-hidden-files t
        treemacs-git-mode 'deferred)
  (evil-define-key 'normal 'global
    (kbd "<leader>tt") #'treemacs
    (kbd "<leader>tf") #'treemacs-find-file))

(use-package treemacs-evil
  :after (treemacs evil))

;; ──────────────────────────────────────────────────────────────
;;  Undo: Vundo
;; ──────────────────────────────────────────────────────────────

(use-package vundo
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "<leader>tu") #'vundo))

;; ──────────────────────────────────────────────────────────────
;;  Terminal: Vterm
;; ──────────────────────────────────────────────────────────────

(defun my/vterm-toggle ()
  "Toggle a vterm buffer."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (bury-buffer)
    (if-let* ((buf (seq-find (lambda (b)
                               (eq (buffer-local-value 'major-mode b) 'vterm-mode))
                             (buffer-list))))
        (switch-to-buffer buf)
      (vterm))))

(use-package vterm
  :after evil
  :config
  (setq vterm-max-scrollback 10000)
  (evil-define-key 'normal 'global
    (kbd "<leader>'") #'my/vterm-toggle))

;; ──────────────────────────────────────────────────────────────
;;  Org-mode
;; ──────────────────────────────────────────────────────────────

(use-package org
  :config
  (setq org-agenda-files '("~/org/")
        org-default-notes-file "~/org/inbox.org"
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("s" "Standup" entry (file+datetree "~/org/standup.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (evil-define-key 'normal 'global
    (kbd "<leader>cc") #'org-capture))

;; ──────────────────────────────────────────────────────────────
;;  Hooks
;; ──────────────────────────────────────────────────────────────

;; Trim trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'init)
;;; init.el ends here
