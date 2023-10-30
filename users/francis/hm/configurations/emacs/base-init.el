;; My emacs init.el
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-verbose nil)
  (setq use-package-always-ensure t))

;; For :general in (use-package)
(require 'general)
;; For :diminish in (use-package).
(use-package diminish)
;; For :bind in (use-package).
(require 'bind-key)

;; Avoid unnecessary regexp matching while loading .el files.
(defvar hm/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun hm/restore-file-name-handler-alist ()
  "Restores the file-name-handler-alist variable."
  (setq file-name-handler-alist hm/file-name-handler-alist)
  (makunbound 'hm/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'hm/restore-file-name-handler-alist)

(setq inhibit-startup-screen t )

(menu-bar-mode 0)
(electric-pair-mode)
(winner-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(when window-system
        (dolist (mode
          '(tool-bar-mode
            tooltip-mode
            scroll-bar-mode
            menu-bar-mode
            blink-cursor-mode))
          (funcall mode 0)))

(add-hook 'text-mode-hook 'auto-fill-mode)

(setq delete-old-versions -1 )
(setq version-control t )
(setq vc-make-backup-files t )
(setq vc-follow-symlinks t )
(setq ring-bell-function 'ignore )

(setq backup-by-copying t)
(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/auto-save-list/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )

(setq require-final-newline t)

(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)
(setq-default fill-column 81)
(column-number-mode)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)
(setq show-paren-delay 0)
(show-paren-mode)

(require 'use-package)
(package-initialize)

(auto-compression-mode 1)
;;;;;;;;;;

;; go
(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
(setq gofmt-command "goimports")
(setq frame-resize-pixelwise t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq default-tab-width 2)
(setq tab-always-indent 'complete)
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

;; For :general in (use-package).
(use-package general
  :ensure t
  :after (evil which-key)
  :config
  (general-evil-setup t)

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

(use-package alchemist)

(use-package better-defaults
			 :config (ido-mode nil))

(use-package company
  :diminish (company-mode)
  :hook ((text-mode prog-mode) . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase completions")
  (company-dabbrev-ignore-case t "Change full casing of completion if completion has different case")
  (company-idle-delay 0.3)
  (comapny-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  (company-transformers '(company-sort-by-backend-importance))
  (company-minimum-prefix-length 2 "Start autocompletion after 2 characters")
  :config
  (add-hook 'after-init-hook 'global-company-mode)
)

(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(eval-after-load 'tramp-sh '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/sh"))
(add-to-list 'tramp-connection-properties
                   (list ".*" "locale" "LC_ALL=C"))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-projectile
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)
(use-package treemacs-magit
  :after (treemacs evil)
  :ensure t)

(require 'dired)
(use-package dired
  :ensure nil
  :config
  ;; dired things
  (progn
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))
(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
    ("<enter>" ."mhj/dwim-toggle-or-open")
    ("<return>" . "mhj/dwim-toggle-or-open")
    ("<tab>" . "mhj/dwim-toggle-or-open")
    ("<down-mouse-1>" . "mhj/mouse-dwim-to-toggle-or-open"))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    )

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
        (mhj/dwim-toggle-or-open))))))

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


(use-package counsel
  :after (general)
  :diminish (counsel-mode)
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
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  (setq ivy-extra-directories '("./"))
  (counsel-mode 1)
)

(use-package cython-mode)

(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package direnv
  :config
  (direnv-mode)
)

(use-package dockerfile-mode)

(use-package elixir-mode
  :mode "\\.ex'")

;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu)

(use-package evil
  :diminish (evil-collection-unimpaired-mode)
  :custom
  (evil-want-keybinding nil "Disable default evil keybindings, since
    evil-collection is a superset. See
    https://github.com/emacs-evil/evil-collection/issues/60.")
  (evil-want-integration t "Also needed for evil-collection")
  :config
  (evil-mode 1)
  ;(evil-define-key '(motion normal visual) 'global
  ;  "n" 'evil-next-line
  ;  "gn" 'evil-next-visual-line
  ;  "e" 'evil-previous-line
  ;  "E" 'evil-lookup
  ;  "ge" 'evil-previous-visual-line
  ;  "i" 'evil-forward-char
  ;  "I" 'evil-window-bottom
  ;  "zi" 'evil-scroll-column-right
  ;  "zI" 'evil-scroll-right
  ;  "j" 'evil-forward-word-end
  ;  "J" 'evil-forward-WORD-end
  ;  "gj" 'evil-backward-word-end
  ;  "gJ" 'evil-backward-WORD-end
  ;  "k" (if (eq evil-search-module 'evil-search) 'evil-ex-search-next 'evil-search-next)
  ;  "K" (if (eq evil-search-module 'evil-search) 'evil-ex-search-previous 'evil-search-previous)
  ;  "gk" 'evil-next-match
  ;  "gK" 'evil-previous-match)
  ;(evil-define-key 'normal 'global
  ;  "l" 'evil-undo
  ;  "u" 'evil-insert
  ;  "U" 'evil-insert-line
  ;  "gu" 'evil-insert-resume
  ;  "gU" 'evil-insert-0-line)
)

(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-mode-alist (delete 'go-mode evil-collection-mode-list))
  (evil-collection-init)
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
)

(use-package flycheck
  :diminish (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :config (global-flycheck-mode)
  :custom
  (flycheck-display-errors-function 'ignore)
  (flycheck-highlighting-mode nil)
  (flycheck-navigation-minimum-level 'error)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
)

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-mode
  :hook
  (go-mode . (lambda ()
    (lsp)
    ;; (lsp-semantic-tokens-mode)
    ;; (add-hook 'before-save-hook 'lsp-organize-imports t t)
    ;; (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (subword-mode 1)
    (define-key evil-motion-state-local-map (kbd "gsff") #'go-goto-function)
    (define-key evil-motion-state-local-map (kbd "gsfa") #'go-goto-arguments)
    (define-key evil-motion-state-local-map (kbd "gsfn") #'go-goto-function-name)
    (define-key evil-motion-state-local-map (kbd "gsfr") #'go-goto-method-receiver)
    (define-key evil-motion-state-local-map (kbd "gsfv") #'go-goto-return-values)
    (define-key evil-motion-state-local-map (kbd "gsd")  #'go-goto-docstring)
    (define-key evil-motion-state-local-map (kbd "gsi")  #'go-goto-imports)
  ))
  :config
  (evil-add-command-properties #'go-goto-arguments :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-docstring :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-function-name :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-method-receiver :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-return-values :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-imports :jump t :repeat 'motion :type 'exclusive)
  (evil-add-command-properties #'go-goto-function :jump t :repeat 'motion :type 'exclusive)
  :custom
  (godoc-at-point-function 'godoc-gogetdoc)
  (gofmt-command "goimports")
  (gofmt-show-errors 'buffer)
)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t)
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
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-height 20)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-virtual-abbreviate 'full)
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

(use-package eglot
  :ensure t
  :defer t
  :config
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 50000)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
  :hook (python-mode . eglot-ensure)
)
;; (use-package lsp-mode
;;   :commands (lsp)
;;   :hook (go-mode . lsp)
;;   :hook (rust-mode . lsp)
;;   :hook (lsp-mode . lsp-enable-which-key-integration)
;;   :config
;;   (lsp-register-custom-settings
;;     '(("gopls.importShortcut" "Definition" nil)
;;       ("gopls.staticcheck" nil t)
;;       ;; ("gopls.semanticTokens" t t)
;;       ("gopls.experimentalPostfixCompletions" t t)))
;;   :custom
;;   (lsp-signature-render-documentation nil)
;;   (lsp-file-watch-threshold nil)
;;   (lsp-auto-execute-action nil)
;;   (lsp-lens-enable t)
;;   (lsp-go-hover-kind "FullDocumentation")
;;   (lsp-rust-server 'rust-analyzer)
;; )
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;         (require 'lsp-pyright)
;;         (lsp)))
;; )
;; (use-package lsp-ui
;;   :after (lsp)
;;   :hook (lsp-mode . lsp-ui-mode)
;; )
;; (use-package lsp-ivy
;;   :after (lsp ivy)
;;   :commands (lsp-ivy-workspace-symbol))

(use-package magit
  :after (general)
  :general
  (general-nmap
  :prefix "SPC"
  "g" '(:ignore t :which-key "Git")
  "gs" 'magit-status)
  :config
  (global-git-commit-mode 1)
  (magit-auto-revert-mode nil)
  (magit-save-repository-buffers 'dontask))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(use-package neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package neuron-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package org)

(use-package org-download)
(use-package org-roam)
(use-package org-roam-ui)
(use-package vertico)

(use-package org-mime)

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

(use-package rust-mode
  :defer t
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
  :defer t
  :mode "\\.toml\\'")

(use-package puppet-mode
  :defer t
  :mode "\\.puppet\\'")

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

(use-package jsonnet-mode
  :defer t
  :mode "\\.jsonnet\\'")

(use-package nov
  :defer t
  :mode "\\.epub\\'")

(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

(use-package virtualenvwrapper)

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.tmpl\\'")

(use-package swiper)

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

(use-package js2-mode
  :mode "\\.js\\'")

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  :bind (
  ("<tab>" . nil)
  ("TAB" . nil)
  ("M-TAB" . "yas-expand"))
)

(setq read-process-output-max (* 1024 1024))
(setq initial-scratch-message "coi")

;; line numbers
(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))

;; tweak some parameters
(set-frame-parameter (selected-frame) 'alpha '(100 . 90))
(add-to-list 'default-frame-alist '(alpha . (100 . 90)))
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)

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
