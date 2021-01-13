;;; hm-init.el --- Emacs configuration à la Home Manager.
;;
;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; A configuration generated from a Nix based configuration by
;; Home Manager.
;;
;;; Code:



(defun hm/reduce-gc ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(defun hm/restore-gc ()
  "Restore the frequency of garbage collection."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Make GC more rare during init and while minibuffer is active.
(eval-and-compile #'hm/reduce-gc)
(add-hook 'minibuffer-setup-hook #'hm/reduce-gc)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'hm/restore-gc)
(add-hook 'minibuffer-exit-hook #'hm/restore-gc)

;; Avoid unnecessary regexp matching while loading .el files.
(defvar hm/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun hm/restore-file-name-handler-alist ()
  "Restores the file-name-handler-alist variable."
  (setq file-name-handler-alist hm/file-name-handler-alist)
  (makunbound 'hm/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'hm/restore-file-name-handler-alist)


(when window-system
  (set-frame-font "Hack 15"))
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

; tweak some parameters
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; go
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setq gofmt-command "goimports")
(setq frame-resize-pixelwise t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq default-tab-width 2)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; rust
(setq rust-format-on-save t)

; extra functions for emacs
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


(eval-when-compile
  (require 'package)

  (setq package-archives nil
        package-enable-at-startup nil
        package--init-file-ensured t)

  (require 'use-package)

  ;; To help fixing issues during startup.
  (setq use-package-verbose t))
;; For :diminish in (use-package).
(require 'diminish)
;; For :bind in (use-package).
(require 'bind-key)
;; For :general in (use-package).
(use-package general
  :config
  (general-evil-setup))

(use-package alchemist)

(use-package better-defaults)

(use-package column-enfore-mode)

(use-package company
  :diminish (company-mode)
  :config
  (company-mode)
)

(use-package counsel
  :after (general)
  :bind* (
    ("C-c /" . counsel-rg)
    ("C-c f" . counsel-git)
    ("C-c l" . counsel-locate)
    ("C-c s" . counsel-git-grep)
    ("C-x C-f" . counsel-find-file)
    ("C-x C-r" . counsel-recentf)
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)
  )
  :general
  (general-nmap
  :prefix "SPC"
  "SPC" '(counsel-M-x :which-key "M-x")
  "ff"  '(counsel-find-file :which-key "find file")
  "s"   '(:ignore t :which-key "search")
  "sc"  '(counsel-unicode-char :which-key "find character"))
)

(use-package cython-mode)

(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package direnv
  :config
  (direnv-mode)
)

(use-package dockerfile-mode)

(use-package elxir-mode
  :mode "\\.ex'")

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)

  :config
  (evil-mode 1)
)

(use-package evil-collection
  :after (evil))

(use-package evil-magit
  :after (magit))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

(use-package flycheck
  :diminish (flycheck-mode)
  :config
  (global-flycheck-mode)
)

(use-package general
  :after (evil which-key)
  :config
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
)

(use-package go-mode
  :config
  (yas-minor-mode-on)
)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
)

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

(use-package ivy
  :after (general)
  :demand t
  :diminish (ivy-mode)
  :config
  (ivy-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-hight 20
      ivy-count-format "(%d/%d) "
      ivy-initial-inputs-alist nil)

  :general
  (general-nmap
  :prefix "SPC"
  "bb" '(ivy-switch-buffer :which-key "switch buffer")
  "fr" '(ivy-recentf :which-key "recent file"))
)

(use-package ledger-mode
  :mode "\\.journal\\'"
  :config
  (setq ledger-reconcile-default-commodity "EUR")
)

(use-package lsp-ivy
  :after (lsp ivy)
  :commands (lsp-ivy-workspace-symbol))

(use-package lsp-mode
  :commands (lsp)
  :hook (go-mode . lsp)
  :hook (rust-mode . lsp)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-rust-server 'rust-analyzer)
)

(use-package lsp-ui
  :after (lsp)
  :commands (lsp-ui-mode))

(use-package magit
  :after (general)
  :general
  (general-nmap
  :prefix "SPC"
  "g" '(:ignore t :which-key "Git")
  "gs" 'magit-status)
)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(use-package neotree)

(use-package neuron-mode)

(use-package nix)

(use-package nix-drv-mode
  :mode "\\.drv\\'")

(use-package nix-mode
  :bind (:map nix-mode-map
    ("C-i" . nix-indent-line)
  )
  :mode "\\.nix\\'")

(use-package nix-prettify-mode
  :config
  (nix-prettify-global-mode)
)

(use-package nlinum-relative
  :after (evil)
  :config
  (nlinum-relative-setup-evil)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(add-hook 'org-mode-hook 'nlinum-relative-mode)
)

(use-package ob)

(use-package org)

(use-package org-download)

(use-package org-mime)

(use-package org-plus-contrib)

(use-package org-pomodoro)

(use-package org-projectile)

(use-package projectile
  :after (ivy general)
  :diminish (projectile-mode)
  :config
  (projectile-mode 1)
(progn
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store"))

  :general
  (general-nmap
  :prefix "SPC"
  "p"  '(:ignore t :which-key "Project")
  "pf" '(projectile-find-file :which-key "Find in project")
  "pl" '(projectile-switch-project :which-key "Switch project"))
)

(use-package protobuf-mode)

(use-package puppet-mode)

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package swiper
  :after (general)
  :bind* (
    ("C-s" . swiper)
  )
  :general
  (general-nmap
  :prefix "SPC"
  "ss" '(swiper :which-key "swiper"))
)

(use-package systemd)

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package virtualenvwrapper)

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.tmpl\\'")

(use-package which-key
  :diminish (which-key-mode)
  :config
  (which-key-mode)
(which-key-setup-side-window-right-bottom)
(setq which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-max-width 0.33
      which-key-idle-delay 0.05)
)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")




(provide 'hm-init)
;; hm-init.el ends here
