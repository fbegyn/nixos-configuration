;;; package --- Summary
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
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

(require 'use-package)
(package-initialize)

;; emacs settings
(use-package emacs
  :ensure nil
  :custom
  ;; vertico
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; corfu
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; disable scratch message
  (setq initial-scratch-message nil)
  ;; switch to y/n prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
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
    (setq mac-command-modifier 'super
          mac-option-modifier 'meta
          mac-control-modifier 'control) ; control as control
    ;; Set path for darwin
    (setenv "PATH" (concat (getenv "PATH") ":/Users/francis/.nix-profile/bin:/usr/bin:/etc/profiles/per-user/francis/bin:/run/current-system/sw/bin"))
    (setq exec-path (append '("/Users/francis/bin" "/profile/bin" "/Users/francis/.npm-packages/bin" "/Users/francis/.nix-profile/bin" "/nix/var/nix/profiles/default/bin" "/usr/local/bin" "/usr/bin" "/Users/francis/.go/bin" "/Users/francis/.local/bin" "/Users/francis/.cargo/bin" "/etc/profiles/per-user/francis/bin" "/run/current-system/sw/bin") exec-path)))
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
        coding-system-for-write 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8
        default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-terminal-coding-system 'utf-8)
  ;; Clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; (defun crm-indicator (args)
  ;;   (cons (format "[CRM%s] %s"
  ;;                 (replace-regexp-in-string
  ;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                  crm-separator)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; better history provision
  (savehist-mode)

  ;; Should use:
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  ;; at least once per installation or while changing this list
  ;; (setq treesit-language-source-alist
  ;;  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
  ;;    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
  ;;    (bash "https://github.com/tree-sitter/tree-sitter-bash")
  ;;    (cmake "https://github.com/uyha/tree-sitter-cmake")
  ;;    (css "https://github.com/tree-sitter/tree-sitter-css")
  ;;    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  ;;    (go "https://github.com/tree-sitter/tree-sitter-go")
  ;;    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
  ;;    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
  ;;    (html "https://github.com/tree-sitter/tree-sitter-html")
  ;;    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  ;;    (json "https://github.com/tree-sitter/tree-sitter-json")
  ;;    (make "https://github.com/alemuller/tree-sitter-make")
  ;;    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  ;;    (python "https://github.com/tree-sitter/tree-sitter-python")
  ;;    (toml "https://github.com/tree-sitter/tree-sitter-toml")
  ;;    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  ;;    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  ;;    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Inhibit startup screens
  (setq inhibit-startup-screen t
        inhibit-splash-screen t)

  ;; Disable some menu elements
  (menu-bar-mode 0)
  (electric-pair-mode)
  (winner-mode 1)

  (recentf-mode 1)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (when window-system
    (dolist (mode '(
                    tool-bar-mode
                    tooltip-mode
                    scroll-bar-mode
                    menu-bar-mode
                    blink-cursor-mode)
                  )
      (funcall mode -1)))

  (add-hook 'text-mode-hook 'auto-fill-mode)

  (setq require-final-newline t)
  (setq sentence-end-double-space nil)
  (setq-default fill-column 81)
  (column-number-mode)
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left)
  (setq show-paren-delay 0)
  (show-paren-mode)

  ;; nixos path adding
  (add-to-list 'exec-path "/home/francis/.nix-profile/bin")
  (add-to-list 'exec-path "/etc/profiles/per-user/francis/bin")
  (add-to-list 'exec-path "/run/current-system/sw/bin")

  (auto-compression-mode 1)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
  (when (daemonp)
      (exec-path-from-shell-initialize))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
;;; end of general emacs configuration

;;; FUNCTIONS
;;; extra functions for emacs
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                      str)
    (setq str (replace-match "" t t str)))
  str)

(defun eshell/e (arg)
  "Opens a given file in Emacs from eshell.
ARG filename to open"
  (find-file arg))

(defun eshell/eh (arg)
  "Opens a file in Emacs from shell horizontally.
ARG filename to open"
  (split-window-vertically)
  (other-window 1)
  (find-file arg))

(defun eshell/ev (arg)
  "Opens a file in Emacs from shell vertically.
ARG filename to open"
  (split-window-horizontally)
  (other-window 1)
  (find-file arg))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;;; ===============================================
;;; packages
;;; ===============================================
(use-package gcmh
	:demand
	:config
	(gcmh-mode 1))

;; For :general in (use-package).
(use-package general
  :demand ;; no lazy loading
  :config
  (general-evil-setup)

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

    "s"   '(:ignore t :which-key "search")

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
    "qq" '(save-buffers-kill-emacs :which-key "quit"))

)

(use-package exec-path-from-shell
  :demand
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  :general
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
)

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
    (setq projectile-enable-caching t
          projectile-require-project-root nil)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store"))
  :general
  (fb/leader-keys
    :states 'normal
    "pf" '(projectile-find-file :which-key "Find in project")
    "pg" '(projectile-ripgrep :which-key "Grep in project")
    ;; Buffers
    "bb" '(projectile-switch-to-buffer :which-key "switch buffer")
    ;; Projects
    "p"  '(:ignore t :which-key "project")
    "p<escape>" '(keyboard-escape-quit :which-key t)
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-switch-project :which-key "add project")
    "pr" '(projectile-switch-project :which-key "remove project")
    ;; run vterm
    "pt" '(projectile-run-vterm :which-key "terminal")))

(use-package rg
  :general
  (fb/leader-keys
    "sr"  '(rg :which-key "rg")
    "sm"  '(rg-menu :which-key "rg-menu"))
  )

;; veritco completion
(use-package vertico
  :demand
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
)
;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrjp)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package vundo)

;; git stuff
(use-package magit
  :after (general)
  :general
  (fb/leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log")
    "g r" '(revert-buffer-quick :which-key "revert buffer")
    "g b" '(magit-branch-checkout :which-key "checkout branch"))
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
  :config
  (define-key treemacs-mode-map [drag-mouse-1] nil)
)
(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (evil-define-key 'treemacs treemacs-mode-map [drag-mouse-1] nil)
)
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
(setq evil-want-keybinding nil
      evil-want-integration t)
  :config
  (evil-mode 1)
)
(use-package evil-collection
  :demand
  :ensure t
  :after (evil)
  :config
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
      "Tangle the literate config and reload."
      (interactive)
      (require 'org)
      (org-babel-tangle-file "~/.emacs.d/init.el")
      (restart-emacs))
  (defun ab/edit-emacs-config ()
      "Open the literate config."
      (interactive)
      (find-file "~/.emacs.d/init.el")))


(use-package deno-ts-mode)

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/home/francis/.nix-profile/bin")
(add-to-list 'tramp-remote-path "/etc/profiles/per-user/francis/bin")
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
(eval-after-load 'tramp-sh '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/sh"))
(add-to-list 'tramp-connection-properties
    (list ".*" "locale" "LC_ALL=C"))
(tramp-set-completion-function
 "ssh" (append (tramp-get-completion-function "ssh")
               (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                       (directory-files
                        "~/.ssh/conf.d/"
                        'full directory-files-no-dot-files-regexp))))

(use-package better-defaults
  :config (ido-mode nil))

;; ----====-----
;; emacs config rework TODO ALL BELOW

;; (use-package company
;;   :diminish (company-mode)
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-dabbrev-downcase nil "Don't downcase completions")
;;   (company-dabbrev-ignore-case t "Change full casing of completion if completion has different case")
;;   (company-idle-delay 0.2)
;;   (comapny-tooltip-align-annotations t)
;;   (company-tooltip-limit 20)
;;   (company-transformers '(company-sort-by-backend-importance))
;;   (company-minimum-prefix-length 1 "Start autocompletion after 2 characters"))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


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
    "ff"  '(counsel-find-file :which-key "find file"))
  :config
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  (counsel-mode 1)
)

(use-package swiper
  :general
  (fb/leader-keys
    "ss"  '(swiper :which-key "swiper")))

(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package direnv
  :config
  (direnv-mode)
)

(use-package dockerfile-mode)

(use-package elixir-ts-mode
  :mode "\\.ex\\'"
  :mode "\\.exs\\'"
  :mode "\\.heex\\'"
  :mode "\\.eex\\'"
)

(require 'project)
(defun project-find-go-module (dir)
  "Set the project root. DIR directory to search for project"
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Set the project root. PROJECT project to handle"
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-mode
  :hook
  (go-mode . (lambda ()
    (lsp)
    ;; (lsp-semantic-tokens-mode)
    (add-hook 'before-save-hook
    (lambda ()
        (call-interactively 'eglot-code-action-organize-imports))
    nil t)
    (subword-mode 1)
    (define-key evil-motion-state-local-map (kbd "gsff") #'go-goto-function)
    (define-key evil-motion-state-local-map (kbd "gsfa") #'go-goto-arguments)
    (define-key evil-motion-state-local-map (kbd "gsfn") #'go-goto-function-name)
    (define-key evil-motion-state-local-map (kbd "gsfr") #'go-goto-method-receiver)
    (define-key evil-motion-state-local-map (kbd "gsfv") #'go-goto-return-values)
    (define-key evil-motion-state-local-map (kbd "gsd")  #'go-goto-docstring)
    (define-key evil-motion-state-local-map (kbd "gsi")  #'go-goto-imports)
    (add-hook 'go-mode-hook #'eglot-ensure)
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
  :ensure nil
  :config
  :hook ((python-mode . eglot-ensure)
         (deno-ts-mode . eglot-ensure)
         (deno-tsx-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (elixir-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 50000)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(elixir-mode "/Users/francis/.local/bin/elixir-ls/language_server.sh"))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "/Users/francis/.local/bin/elixir-ls/language_server.sh"))
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")
         (analyzer.fillstruct . t)
  ))))
)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package org)
(use-package org-download)
(use-package org-roam)
(use-package org-roam-ui)
(use-package org-mime)
(use-package org-pomodoro)
(use-package org-projectile)

(use-package protobuf-mode)

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

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

;; (use-package nov
;;   :defer t
;;   :mode "\\.epub\\'")

(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

(use-package virtualenvwrapper)

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.tmpl\\'")

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

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq frame-resize-pixelwise t)

(add-hook 'project-find-functions #'project-find-go-module)

(provide 'base-init)
;;; base-init.el ends here
