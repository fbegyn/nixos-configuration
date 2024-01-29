
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

;; Inhibit startup screens
(setq inhibit-startup-screen t )
(setq inhibit-splash-screen t )

;; Disable some menu elements
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
          (funcall mode -1)))

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

;; emacs settings
(use-package emacs
  :defer t
  :init
  ;; disable scratch message
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))
  ;; switch to y/n prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; use spaces and set tab width
  (setq-default indent-tabs-mode nil)
  (setq default-tab-width 2)
  ;; Stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)
  
  ;; Follow symlinks
  (setq vc-follow-symlinks t)
  ;; correct macos modifiers
  (defun fb/is-macos? ()
        (eq system-type 'darwin))
  (when (fb/is-macos?)
        (setq mac-command-modifier 'super)   ; command as super
        (setq mac-option-modifier 'meta)     ; alt as meta
        (setq mac-control-modifier 'control)) ; control as control
  ;; emacs-mac
  (when (fboundp 'mac-auto-operator-composition-mode)
        (mac-auto-operator-composition-mode) ; enables font ligatures
        (global-set-key [(s c)] 'kill-ring-save)
        (global-set-key [(s v)] 'yank)
        (global-set-key [(s x)] 'kill-region)
        (global-set-key [(s q)] 'kill-emacs))
  ;; use escape to exit menus (like vim)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; use UTF-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; Less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  ;; Clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)
)


(auto-compression-mode 1)
;;;;;;;;;;

;; bibliography listings
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; highlight guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(setq tab-always-indent 'complete)

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

;; ===============================================
(use-package gcmh
	:demand
	:config
	(gcmh-mode 1))

;; For :general in (use-package).
(use-package general
  :demand ;; no lazy loading
  :config
  (general-evil-setup)

  ;;(general-mmap
  ;;  ":" 'evil-ex
  ;;  ";" 'evil-repeat-find-char)

  (general-create-definer fb/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (fb/leader-keys
      "b"  '(:ignore t :which-key "buffer")
      "bd" '(kill-this-buffer :which-key "kill buffer")

      "f"  '(:ignore t :which-key "file")
      "ff" '(find-file :which-key "find")
      "fs" '(save-buffer :which-key "save")

      "m" '(:ignore t :which-key "mode")

      "k"   '(:ignore t :which-key "kubernetes")
      "kk"  '(kubel t :which-key "kubel")

      "t"  '(:ignore t :which-key "toggle")
      "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
      "tt" '(treemacs :which-key "treemacs")
      "wv" '(split-window-horizontally :which-key "split vertical")
      "ws" '(split-window-vertically :which-key "split horizontal")
      "wk" '(evil-window-up :which-key "up")
      "wj" '(evil-window-down :which-key "down")
      "wh" '(evil-window-left :which-key "left")
      "wl" '(evil-window-right :which-key "right")
      "wd" '(delete-window :which-key "delete")

      "q"  '(:ignore t :which-key "quit")
      "qq" '(save-buffers-kill-emacs :which-key "quit")))
(use-package which-key
  :demand
  :diminish (which-key-mode)
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.25))
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)

(use-package projectile
  :demand
  :after (general)
  :init
  (projectile-mode +1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store"))
  :general
  (fb/leader-keys
    :states 'normal
    "pf" '(projectile-find-file :which-key "Find in project")
    ;; Buffers
    "bb" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p"  '(:ignore t :which-key "project")
    "p<escape>" '(keyboard-escape-quit :which-key t)
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-switch-project :which-key "add project")
    "pr" '(projectile-switch-project :which-key "remove project")))

(use-package ivy
  :demand
  :after (general)
  :config
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-height 20)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-virtual-abbreviate 'full)
  :general
  (fb/leader-keys
    "bb" '(ivy-switch-buffer :which-key "switch buffer")
    "fr" '(ivy-recentf :which-key "recent file")))

;; git stuff
(use-package magit
  :after (general)
  :general
  (fb/leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
    "g r" '(revert-buffer-quick :which-key "revert buffer"))
  (general-nmap
    "<escape>" #'transient-quit-one)
  :config
  (global-git-commit-mode 1)
  ;; (magit-auto-revert-mode nil)
  (magit-save-repository-buffers 'dontask))
(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package vterm
  :demand)
(use-package vterm-toggle
  :general
  (fb/leader-keys
    "'" '(vterm-toggle :which-key "terminal")))

;; look and feel
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-hard t))
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?|)
  (setq highlight-indent-guides-responsive 'top))

(use-package nerd-icons)
(use-package all-the-icons)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-all-the-icons
  :after treemacs)

(use-package evil
  :demand
  :diminish (evil-collection-unimpaired-mode)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))
(use-package evil-collection
  :demand
  :ensure t
  :after (evil)
  :config
  (setq evil-collection-mode-alist (delete 'go-mode evil-collection-mode-list))
  (evil-collection-init))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "gc" 'evilnc-comment-operator))

(use-package flycheck
  :diminish (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :config (global-flycheck-mode)
  :custom
  (flycheck-display-errors-function 'ignore)
  (flycheck-highlighting-mode nil)
  (flycheck-navigation-minimum-level 'error)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package restart-emacs
	:general
	(fb/leader-keys
		"e" '(:ignore true :wk "emacs")
		"e <escape>" '(keyboard-escape-quit :wk t)
		"e e" '(ab/edit-emacs-config :wk "edit")
		"e R" '(restart-emacs :wk "restart")
		"e L" '(ab/reload-emacs :wk "reload"))
	:init
	(defun ab/reload-emacs ()
		"Tangle the literate config and reload"
		(interactive)
		(require 'org)
		(org-babel-tangle-file "~/.emacs.d/init.el")
		(restart-emacs)
		)
  (defun ab/edit-emacs-config ()
		"Open the literate config"
		(interactive)
		(find-file "~/.emacs.d/init.el")))

(use-package kubel
  :defer t
  :after (vterm)
  :config
  (kubel-vterm-setup))
(use-package kubel-evil
  :defer t
  :after (evil kubel))

;; ----====-----
;; emacs config rework TODO ALL BELOW

(use-package alchemist)

(use-package better-defaults
  :config (ido-mode nil))

(use-package company
  :diminish (company-mode)
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase completions")
  (company-dabbrev-ignore-case t "Change full casing of completion if completion has different case")
  (company-idle-delay 0.3)
  (comapny-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  (company-transformers '(company-sort-by-backend-importance))
  (company-minimum-prefix-length 2 "Start autocompletion after 2 characters"))

(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(eval-after-load 'tramp-sh '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/sh"))
(add-to-list 'tramp-connection-properties
                   (list ".*" "locale" "LC_ALL=C"))

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
  (fb/leader-keys
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

(use-package ledger-mode
  :mode "\\.journal\\'"
  :config
  (setq ledger-reconcile-default-commodity "€")
)

(use-package eglot
  :ensure t
  :config
  :hook (python-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 50000)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
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
  (fb/leader-keys
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
