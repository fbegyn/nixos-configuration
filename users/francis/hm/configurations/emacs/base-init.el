;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-verbose nil)
  (setq use-package-always-ensure t))

(require 'general)
(require 'use-package)
(package-initialize)

;; Defer every package by default; packages that must be active at startup
;; say so with `:demand t' (or load on idle with `:defer N').
(setq use-package-always-defer t)

;; For :diminish in (use-package).
(use-package diminish :ensure t :demand t)
;; For :bind in (use-package).
(require 'bind-key)

;; emacs settings
(use-package emacs
  :ensure t
  :demand t
  :custom
  ;; vertico
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  :init
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
    ;; PATH is imported from the login shell by `exec-path-from-shell' below.
    ;; Use GNU ls (gls, from coreutils-prefixed) so dired's --dired switch
    ;; and the GNU-only flags in `dired-listing-switches` work on macOS.
    (setq insert-directory-program "gls"))
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
        default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-terminal-coding-system 'utf-8)
  ;; Clean up the mode line
  (display-time-mode 1)

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

  ;; better history provision
  (setq history-length 100)
  (savehist-mode)

  ;; Inhibit startup screens
  (setq inhibit-startup-screen t
        inhibit-splash-screen t
        initial-scratch-message "coi")

  ;; Disable some menu elements
  (menu-bar-mode 0)
  (electric-pair-mode)
  (winner-mode 1)

  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never)
  (recentf-mode 1)

  (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-width-start t)
  ;; Line numbers are enabled per-mode below (prog/text/conf), not globally,
  ;; so they never render in dired/special buffers.

  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (when window-system
    (dolist (mode '(tool-bar-mode
                    tooltip-mode
                    scroll-bar-mode
                    menu-bar-mode
                    blink-cursor-mode))
      (funcall mode -1)))

  (add-hook 'text-mode-hook 'auto-fill-mode)

  (setq require-final-newline t)
  (setq sentence-end-double-space nil)
  (setq-default fill-column 101)
  (column-number-mode)
  ;; Scope the fill-column indicator and trailing-whitespace highlighting to
  ;; code buffers instead of everywhere (lighter, no noise in special buffers).
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))
  (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left)
  (setq show-paren-delay 0)
  (show-paren-mode)

  ;; nixos path adding
  (unless (fb/is-macos?)
    (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
    (add-to-list 'exec-path (concat "/etc/profiles/per-user/" user-login-name "/bin"))
    (add-to-list 'exec-path "/run/current-system/sw/bin"))

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

  ;; line numbers - scoped to relevant modes instead of global
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)

  ;; Set the font
  (set-frame-font (if (eq system-type 'darwin) "Menlo 14" "DejaVu Sans Mono 14") nil t)

  ;; tweak some parameters
  (set-frame-parameter (selected-frame) 'alpha '(100 . 90))
  (add-to-list 'default-frame-alist '(alpha . (100 . 90)))
  (setq frame-inhibit-implied-resize t)
  (pixel-scroll-precision-mode 1)
  (setq frame-resize-pixelwise t)
  (setq read-process-output-max (* 4 1024 1024))
  ;; Redisplay / rendering work we don't need.
  (setq redisplay-skip-fontification-on-input t)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right
                cursor-in-non-selected-windows nil
                pgtk-wait-for-event-timeout 0)
  (setq bidi-inhibit-bpa t
        highlight-nonselected-windows nil)
  ;; Don't resolve hostnames when running `find-file-at-point'.
  (setq ffap-machine-p-known 'reject)

  ;; Sane defaults previously provided by `better-defaults'.
  (setq-default indent-tabs-mode nil)
  (setq uniquify-buffer-name-style 'forward
        save-interprogram-paste-before-kill t
        mouse-yank-at-point t
        apropos-do-all t
        ediff-window-setup-function 'ediff-setup-windows-plain)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward))
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

;; For :general in (use-package).
(use-package general
  :demand t ;; no lazy loading
  :config
  (general-evil-setup)

  (general-create-definer fb/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (fb/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")

    "b"  '(:ignore t :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")

    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find")
    "fr" '(consult-recent-file :which-key "recent")
    "fd" '(dired-jump :which-key "dired here")
    "fs" '(save-buffer :which-key "save")

    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "search line")
    "sr"  '(consult-ripgrep :which-key "ripgrep")
    "si"  '(consult-imenu :which-key "imenu")

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
    "qq" '(save-buffers-kill-emacs :which-key "quit")))

;;; ===============================================
;;; packages
;;; ===============================================
;; On Linux the home-manager emacs service starts the daemon through a login
;; shell, so PATH and friends are already inherited; only macOS needs this.
(use-package exec-path-from-shell
  :if (fb/is-macos?)
  :demand t
  :ensure t
  :config
  ;; Use bash in login-only mode so we skip the interactive zsh stack
  ;; (which exec's fish and trips `setopt zle` in non-interactive context).
  (setq exec-path-from-shell-shell-name "bash"
        exec-path-from-shell-arguments '("-l"))
  (dolist (var '("PATH" "GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; ──────────────────────────────────────────────────────────────
;;  Evil
;; ──────────────────────────────────────────────────────────────
(use-package evil
  :demand t
  :diminish (evil-collection-unimpaired-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-C-u-scroll t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

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
;;  Tree-sitter
;; ──────────────────────────────────────────────────────────────
;; Grammars come from nix (treesit-grammars in default.nix). Every remap is
;; guarded on the grammar loading, so a missing or ABI-incompatible grammar
;; falls back to the classic mode instead of erroring.
(defun fb/ts-remap (remaps)
  "Add each (LANG FROM TO) in REMAPS to `major-mode-remap-alist' if LANG loads."
  (pcase-dolist (`(,lang ,from ,to) remaps)
    (when (treesit-language-available-p lang)
      (add-to-list 'major-mode-remap-alist (cons from to)))))

(defun fb/ts-auto-mode (entries)
  "Add each (LANG REGEXP MODE) in ENTRIES to `auto-mode-alist' if LANG loads."
  (pcase-dolist (`(,lang ,regexp ,mode) entries)
    (when (treesit-language-available-p lang)
      (add-to-list 'auto-mode-alist (cons regexp mode)))))

(fb/ts-remap
 '((python python-mode    python-ts-mode)
   (bash   sh-mode        bash-ts-mode)
   (json   js-json-mode   json-ts-mode)
   (json   json-mode      json-ts-mode)
   (toml   conf-toml-mode toml-ts-mode)))

(fb/ts-auto-mode
 '((go         "\\.go\\'"                                        go-ts-mode)
   (gomod      "/go\\.mod\\'"                                    go-mod-ts-mode)
   (dockerfile "\\(?:Docker\\|Container\\)file\\(?:\\..*\\)?\\'" dockerfile-ts-mode)
   (yaml       "\\.ya?ml\\'"                                     yaml-ts-mode)
   (elixir     "\\.exs?\\'"                                      elixir-ts-mode)
   (heex       "\\.[hl]?eex\\'"                                  heex-ts-mode)))

(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-mode-indent-offset 4)
  :hook (go-ts-mode . subword-mode))

(use-package elixir-ts-mode :ensure nil)
(use-package heex-ts-mode :ensure nil)

;; Ansible: yaml-ts-mode plus ansible-language-server, selected per file.
(define-derived-mode fb/ansible-mode yaml-ts-mode "Ansible"
  "YAML mode for Ansible playbooks, roles and inventories.")

(defun fb/ansible-file-p ()
  "Return non-nil if the current buffer looks like an Ansible file."
  (when-let* ((file buffer-file-name))
    (or (string-match-p "/\\(?:roles\\|playbooks\\|tasks\\|handlers\\|group_vars\\|host_vars\\)/" file)
        (locate-dominating-file file "ansible.cfg"))))

(defun fb/maybe-ansible-mode ()
  "Switch to `fb/ansible-mode' when visiting an Ansible file."
  (when (and (not (derived-mode-p 'fb/ansible-mode))
             (not (file-remote-p default-directory))
             (fb/ansible-file-p))
    (fb/ansible-mode)))
(add-hook 'yaml-ts-mode-hook #'fb/maybe-ansible-mode)

;; Vim text objects / motions on tree-sitter nodes: af/if, ac/ic, ]f/[f.
(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-normal-state-map (kbd "]f")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[f")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))))

;; ──────────────────────────────────────────────────────────────
;;  Theme & UI
;; ──────────────────────────────────────────────────────────────
(use-package gruvbox-theme
  :demand t
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package nerd-icons
  :demand t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-lsp t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-minor-modes nil)
  (doom-modeline-modal-icon t)
  ;; nil: t spawns `python --version' / `go version' per buffer.
  (doom-modeline-env-version nil))

(use-package tab-line
  :ensure nil
  :demand t
  :init
  (defun fb/tab-line-buffers ()
    "Return file-visiting buffers shown as tabs."
    (seq-filter
     (lambda (b)
       (and (buffer-file-name b)
            (not (string-prefix-p " " (buffer-name b)))))
     (buffer-list)))

  (defun fb/tab-line-name-format (tab tabs)
    "Render TAB with a nerd-icons file-type icon + buffer name."
    (let* ((buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
           (name   (buffer-name buffer))
           (icon   (with-current-buffer buffer
                     (nerd-icons-icon-for-buffer)))
           (face   (if (eq buffer (window-buffer))
                       'tab-line-tab-current
                     'tab-line-tab-inactive)))
      (propertize (format " %s %s "
                          (if (stringp icon) icon "")
                          name)
                  'face face
                  'mouse-face 'tab-line-highlight)))

  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show t)
  (tab-line-separator "")
  (tab-line-tabs-function #'fb/tab-line-buffers)
  (tab-line-tab-name-format-function #'fb/tab-line-name-format)
  (tab-line-exclude-modes '(dired-mode
                            dired-sidebar-mode
                            magit-status-mode
                            magit-log-mode
                            magit-diff-mode
                            magit-revision-mode
                            vterm-mode
                            eshell-mode
                            help-mode
                            Info-mode
                            messages-buffer-mode))
  :config
  (global-tab-line-mode 1)
  ;; Gruvbox harmony: blend strip with theme; current tab gets a yellow accent.
  (let ((bg     "#1d2021")
        (bg-alt "#282828")
        (fg     "#ebdbb2")
        (fg-dim "#a89984")
        (accent "#fabd2f"))
    (set-face-attribute 'tab-line              nil :background bg :foreground fg-dim
                        :height 0.95 :box nil :overline bg)
    (set-face-attribute 'tab-line-tab          nil :inherit 'tab-line :box nil)
    (set-face-attribute 'tab-line-tab-inactive nil :background bg :foreground fg-dim :box nil)
    (set-face-attribute 'tab-line-tab-current  nil :background bg-alt :foreground fg
                        :box nil :overline accent :weight 'bold)
    (set-face-attribute 'tab-line-highlight    nil :background bg-alt :foreground fg)))

(use-package which-key
  :demand t
  :diminish (which-key-mode)
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.25
        which-key-show-early-on-C-h t
        which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(use-package highlight-indent-guides
  ;; Only where indentation is semantic; guides cost redisplay time.
  :hook ((yaml-ts-mode python-ts-mode) . highlight-indent-guides-mode)
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
  :demand t
  :custom
  (vertico-count 20)
  (vertico-resize t)
  ;; Different scroll margin
  ;; (vertico-scroll-margin 0)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  ;; vertico-buffer / vertico-multiform ship inside the vertico package.
  (require 'vertico-buffer)
  (vertico-multiform-mode 1)            ;; explicit ON (Embark re-asserts it too)
  ;; Roomy, always-visible candidate list in a full-width bottom window. This
  ;; keeps search candidates visible even when the dired-sidebar is open, where
  ;; the cramped minibuffer cannot grow. `display-buffer-at-bottom' is chosen
  ;; over vertico's default `display-buffer-use-least-recent-window', which would
  ;; commandeer the main window and cover the consult preview.
  (setq vertico-buffer-display-action
        '(display-buffer-at-bottom (window-height . 0.4)))
  ;; Route search-style commands through vertico-buffer.
  (setq vertico-multiform-commands
        '((consult-ripgrep      buffer)
          (consult-grep         buffer)
          (consult-git-grep     buffer)
          (consult-find         buffer)
          (consult-fd           buffer)
          (consult-line         buffer)
          (consult-line-multi   buffer)
          (consult-imenu        buffer)
          (consult-imenu-multi  buffer)
          (consult-project-buffer buffer)
          (project-find-file    buffer)
          (project-find-regexp  buffer)))
)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :demand t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; consult backend completion
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
         ("M-s r" . consult-ripgrep)
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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
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

;; ──────────────────────────────────────────────────────────────
;;  Completion: Corfu + Cape + Dabbrev
;; ──────────────────────────────────────────────────────────────
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

  ;; Corfu is enabled globally below via `global-corfu-mode'; no per-mode hooks
  ;; needed. See `global-corfu-modes' to exclude specific modes.

  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("RET" . corfu-insert))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

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

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :bind (("<tab>" . nil)
         ("TAB" . nil)
         ("M-TAB" . yas-expand)))

;; ──────────────────────────────────────────────────────────────
;;  LSP: Eglot
;; ──────────────────────────────────────────────────────────────
(defun fb/eglot-ensure-maybe ()
  "Start eglot, skipping remote files.
Connecting is already asynchronous (`eglot-sync-connect' is 0)."
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

(defvar fb/nil-auto-eval nil
  "Whether nil_ls autoEvalInputs is enabled.")

(defun fb/eglot-workspace-config (_server)
  "Return workspace configuration for eglot servers."
  (list :gopls (list :staticcheck t :analyses (list :fillstruct t))
        :nil (list :nix (list :flake ( list
                                       :autoEvalInputs (if fb/nil-auto-eval t :json-false)
                                       :autoArchive :json-false )))
        :python (list :pythonPath
                      (or (and (fboundp 'pet-executable-find)
                               (pet-executable-find "python"))
                          "python"))))

(use-package eglot
  :ensure nil
  :hook (((go-ts-mode go-mod-ts-mode
           python-ts-mode python-mode
           bash-ts-mode sh-mode
           dockerfile-ts-mode
           terraform-mode
           yaml-ts-mode
           elixir-ts-mode heex-ts-mode
           nix-mode rust-mode
           deno-ts-mode deno-tsx-mode) . fb/eglot-ensure-maybe))
  :general
  (fb/leader-keys
    "c"  '(:ignore t :which-key "code")
    "c a" '(eglot-code-actions :which-key "eglot code actions")
    "c d" '(consult-flymake :which-key "diagnostics")
    "c n" '(flymake-goto-next-error :which-key "next error")
    "c p" '(flymake-goto-prev-error :which-key "prev error"))
  :custom
  (eglot-autoshutdown t)
  ;; Disable JSON-RPC event logging for performance on chatty servers
  ;; (gopls/rust-analyzer). Set to a positive integer to debug.
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-sync-connect 0)
  (eglot-connect-timeout 60)
  (eglot-report-progress nil)
  (eglot-code-action-indications nil)
  (eglot-extend-to-xref t)
  ;; Low enough that diagnostics track typing; completion requests flush
  ;; pending changes anyway.
  (eglot-send-changes-idle-time 0.5)
  (flymake-no-changes-timeout 1)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider :inlayHintProvider))
  (setq jsonrpc-event-hook nil)
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '((elixir-ts-mode heex-ts-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs '((deno-ts-mode deno-tsx-mode) . ("deno" "lsp")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; Must be added after (= in front of) eglot's built-in yaml-ts-mode entry.
  (add-to-list 'eglot-server-programs '(fb/ansible-mode . ("ansible-language-server" "--stdio")))
  (setq-default eglot-workspace-configuration #'fb/eglot-workspace-config))

(defvar fb/eglot-format-modes
  '(go-ts-mode go-mod-ts-mode elixir-ts-mode heex-ts-mode terraform-mode bash-ts-mode rust-mode)
  "Major modes formatted through eglot on save.
Python formatting is handled by apheleia (ruff); pyright has no formatter.")

(defun fb/eglot-organize-imports ()
  "Organize imports via eglot code actions."
  (when (eglot-managed-p)
    (ignore-errors
      (eglot-code-action-organize-imports (point-min) (point-max)))))

(defun fb/eglot-format-before-save ()
  "Organize imports (Go) and format the buffer via eglot."
  (when (eglot-managed-p)
    (when (derived-mode-p 'go-ts-mode)
      (fb/eglot-organize-imports))
    (eglot-format-buffer)))

;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun fb/eglot-format-on-save-setup ()
  "Install format-on-save in eglot buffers listed in `fb/eglot-format-modes'."
  (when (apply #'derived-mode-p fb/eglot-format-modes)
    (if (eglot-managed-p)
        (add-hook 'before-save-hook #'fb/eglot-format-before-save -10 t)
      (remove-hook 'before-save-hook #'fb/eglot-format-before-save t))))
(add-hook 'eglot-managed-mode-hook #'fb/eglot-format-on-save-setup)

;; Toggle nil autoEvalInputs
(defun fb/toggle-nil-auto-eval ()
  "Toggle nil autoEvalInputs and notify the LSP server."
  (interactive)
  (setq fb/nil-auto-eval (not fb/nil-auto-eval))
  (when-let* ((server (eglot-current-server)))
    (eglot-signal-didChangeConfiguration server))
  (message "nil autoEvalInputs: %s" fb/nil-auto-eval))

;; ──────────────────────────────────────────────────────────────
;;  emacs garbage collection hack
;'  https://github.com/emacsmirror/gcmh
;; ──────────────────────────────────────────────────────────────
(use-package gcmh
	:demand
	:config
	(gcmh-mode 1))

(use-package rg
  :general
  (fb/leader-keys
    "sr"  '(rg :which-key "rg")
    "sm"  '(rg-menu :which-key "rg-menu")))

;; Open a vterm rooted at the current project (replaces projectile-run-vterm).
(defun fb/project-vterm ()
  "Open a vterm in the current project's root directory."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (vterm)))

(use-package project
  :ensure nil
  :after (general)
  :config
  ;; Commands offered by `project-switch-project' (SPC p p).
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-ripgrep "Ripgrep")
          (project-dired "Dired")
          (fb/project-vterm "Vterm")))
  :general
  (fb/leader-keys
    :states 'normal
    ;; Buffers
    "bb" '(consult-project-buffer :which-key "switch buffer")
    ;; Projects
    "p"  '(:ignore t :which-key "project")
    "p<escape>" '(keyboard-escape-quit :which-key t)
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "pg" '(consult-ripgrep :which-key "ripgrep")
    "pb" '(consult-project-buffer :which-key "switch buffer")
    "pd" '(project-dired :which-key "dired")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "pt" '(fb/project-vterm :which-key "terminal")))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :init
  ;; Needed so `git commit' via emacsclient gets git-commit-mode; load it when
  ;; idle instead of at startup.
  (run-with-idle-timer 2 nil (lambda ()
                               (require 'git-commit)
                               (global-git-commit-mode 1)))
  :config
  ;; (magit-auto-revert-mode nil)
  (magit-save-repository-buffers 'dontask))
(defun fb/diff-hl-unless-remote ()
  "Enable diff-hl-mode unless the file is remote."
  (unless (file-remote-p default-directory)
    (diff-hl-mode)))

(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :hook ((prog-mode . fb/diff-hl-unless-remote)
         (text-mode . fb/diff-hl-unless-remote)
         (conf-mode . fb/diff-hl-unless-remote)
         (dired-mode . diff-hl-dired-mode)))

;; jujutsu (jj) version control
(use-package vc-jj
  :after (general)
  :general
  (fb/leader-keys
    "j"  '(:ignore t :which-key "jujutsu")
    "j <escape>" '(keyboard-escape-quit :which-key t)
    "j s" '(vc-dir :which-key "status")
    "j l" '(vc-print-log :which-key "log")
    "j d" '(vc-diff :which-key "diff")
    "j a" '(vc-annotate :which-key "annotate")))

(use-package majutsu
  :after (general)
  :ensure nil
  :commands (majutsu majutsu-status majutsu-log)
  :general
  (fb/leader-keys
    "j j" '(majutsu :which-key "majutsu")
    "j L" '(majutsu-log :which-key "log (majutsu)")))

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (add-hook
	'term-mode-hook
	(lambda() (setq show-trailing-whitespace nil))))
(use-package vterm-toggle
  :general
  (fb/leader-keys
    "'" '(vterm-toggle :which-key "terminal")))

;; (defvar fbegyn:dark-theme 'gruvbox-dark-hard
;;   "Default dark theme.")
;;
;; (defvar fbegyn:light-theme 'gruvbox-light-hard
;;   "Default light theme.")
;;
;; (defun fbegyn:theme-from-dbus (value)
;;   "Change the theme based on a D-Bus property.
;;
;; VALUE should be an integer or an arbitrarily nested list that
;; contains an integer.  When VALUE is equal to 2 then a light theme
;; will be selected, otherwise a dark theme will be selected."
;;   (load-theme (if (= 2 (car (flatten-list value)))
;;                   fbegyn:light-theme
;;                 fbegyn:dark-theme)
;;               t))
;;
;; (require 'dbus)
;;
;; ;; Set the current theme based on what the system theme is right now:
;; (dbus-call-method-asynchronously
;;    :session "org.freedesktop.portal.Desktop"
;;    "/org/freedesktop/portal/desktop"
;;    "org.freedesktop.portal.Settings"
;;    "Read"
;;    #'fbegyn:theme-from-dbus
;;    "org.freedesktop.appearance"
;;    "color-scheme")
;;
;; ;; Register to be notified when the system theme changes:
;; (dbus-register-signal
;;    :session "org.freedesktop.portal.Desktop"
;;    "/org/freedesktop/portal/desktop"
;;    "org.freedesktop.portal.Settings"
;;    "SettingChanged"
;;    (lambda (path var value)
;;      (when (and (string-equal path "org.freedesktop.appearance")
;;                 (string-equal var "color-scheme"))
;;        (fbegyn:theme-from-dbus value))))

;; Diagnostics are handled by flymake (eglot drives it natively); see the
;; flymake leader bindings in the eglot block.

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


;; Registers itself for .ts/.tsx in deno projects via its autoloads.
(use-package deno-ts-mode)

;; TRAMP settings
;; TRAMP is loaded lazily; everything touching its variables lives in
;; `with-eval-after-load' blocks.
;; (setq tramp-default-method "scp")
(with-eval-after-load 'tramp
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/francis/.nix-profile/bin")
  (add-to-list 'tramp-remote-path "/etc/profiles/per-user/francis/bin")
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  (add-to-list 'tramp-connection-properties
               (list ".*" "locale" "LC_ALL=C")))
(defun fb/ssh-conf-files ()
  "Return the ssh client config files to parse for host names.
The first element is ~/.ssh/config; the rest are ~/.ssh/conf.d/*."
  (append (list (expand-file-name "~/.ssh/config"))
          (when (file-directory-p (expand-file-name "~/.ssh/conf.d/"))
            (directory-files (expand-file-name "~/.ssh/conf.d/")
                             'full directory-files-no-dot-files-regexp))))

(defun fb/ssh-host-candidates ()
  "Collect SSH host names from ssh config files and known_hosts.
Sources: ~/.ssh/config, ~/.ssh/conf.d/*, and ~/.ssh/known_hosts.
Reuses TRAMP's parsers so candidates match `/ssh:' TAB completion.
Wildcard patterns and hashed known_hosts entries are skipped."
  (let ((hosts '()))
    (dolist (f (fb/ssh-conf-files))
      (when (file-readable-p f)
        (pcase-dolist (`(,_user ,host) (tramp-parse-sconfig f))
          (when host (push host hosts)))))
    (let ((known (expand-file-name "~/.ssh/known_hosts")))
      (when (file-readable-p known)
        (pcase-dolist (`(,_user ,host) (tramp-parse-shosts known))
          (when host (push host hosts)))))
    (seq-sort
     #'string<
     (seq-uniq
      (seq-remove (lambda (h) (or (string-empty-p h)
                                  (string-match-p "[*?]" h)))
                  hosts)))))

;; Augment TRAMP's default ssh host parsers (which already cover ~/.ssh/config
;; and ~/.ssh/known_hosts) with the ~/.ssh/conf.d/* drop-in files. `cdr' drops
;; ~/.ssh/config since it is already in the defaults.
(with-eval-after-load 'tramp
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh")
                 (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                         (cdr (fb/ssh-conf-files)))))
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

;; making TRAMP go brrrr
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;;; TRAMP leader key bindings
(defun fb/tramp-open-remote-file ()
  "Open a file on a remote SSH host, completing host then path."
  (interactive)
  (let ((host (completing-read "SSH host: " (fb/ssh-host-candidates))))
    (find-file (read-file-name "Find file: " (concat "/ssh:" host ":~/")))))

(defun fb/tramp-open-remote-dir ()
  "Open a directory on a remote SSH host in dired, completing the host."
  (interactive)
  (let ((host (completing-read "SSH host: " (fb/ssh-host-candidates))))
    (dired (concat "/ssh:" host ":~/"))))

(defun fb/tramp-open-remote-sudo-dir ()
  "Open a remote SSH host's root as sudo in dired, completing the host."
  (interactive)
  (let ((host (completing-read "SSH host: " (fb/ssh-host-candidates))))
    (dired (concat "/ssh:" host "|sudo:" host ":/"))))

(defun fb/tramp-open-recent ()
  "Open a recent TRAMP connection."
  (interactive)
  (let* ((recent-files (seq-filter
                        (lambda (f) (file-remote-p f))
                        recentf-list))
         (selected (completing-read "Recent remote: " recent-files nil t)))
    (find-file selected)))

(fb/leader-keys
  "r"   '(:ignore t :which-key "remote")
  "r <escape>" '(keyboard-escape-quit :which-key t)
  "rf"  '(fb/tramp-open-remote-file :which-key "open file")
  "rd"  '(fb/tramp-open-remote-dir :which-key "open dir")
  "rs"  '(fb/tramp-open-remote-sudo-dir :which-key "sudo dir")
  "rr"  '(fb/tramp-open-recent :which-key "recent")
  "rc"  '(tramp-cleanup-all-connections :which-key "cleanup all")
  "rC"  '(tramp-cleanup-connection :which-key "cleanup one"))

;; MAGIT tramp
;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
(setq magit-commit-show-diff nil)
;; don't show git variables in magit branch
(setq magit-branch-direct-configure nil)
;; don't automatically refresh the status buffer after running a git command
(setq magit-refresh-status-buffer nil)


(use-package groovy-mode)
(use-package jenkinsfile-mode)

;; ----====-----
;; emacs config rework TODO ALL BELOW

(use-package dired
  :ensure nil
  :defer t
  :config
  ;; dired things
  (progn
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))
(use-package dired-subtree
  :bind
  (:map dired-mode-map
    ("<enter>" . mhj/dwim-toggle-or-open)
    ("<return>" . mhj/dwim-toggle-or-open)
    ("<tab>" . mhj/dwim-toggle-or-open)
    ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s))))

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

;; Fast file-tree sidebar backed by dired (reuses dired + dired-subtree).
(use-package dired-sidebar
  :after (dired general)
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (defun fb/dired-sidebar-toggle ()
    "Toggle dired-sidebar rooted at the current project (fallback: cwd)."
    (interactive)
    (let ((default-directory
           (or (when-let* ((proj (project-current))) (project-root proj))
               default-directory)))
      (dired-sidebar-toggle-sidebar)))
  :custom
  (dired-sidebar-theme 'ascii)                  ;; no icons; alt: 'none
  (dired-sidebar-width 30)                       ;; narrow
  (dired-sidebar-window-fixed 'width)            ;; fixed width only; height may flex
  (dired-sidebar-should-follow-file t)           ;; auto-follow the open file
  (dired-sidebar-follow-file-idle-delay 0.5)
  (dired-sidebar-pop-to-sidebar-on-toggle-open t)
  (dired-sidebar-no-delete-other-windows t)      ;; survive C-x 1 / window deletes
  :bind (:map dired-sidebar-mode-map
         ("<return>"       . dired-sidebar-find-file)
         ("<enter>"        . dired-sidebar-find-file)
         ("<tab>"          . dired-sidebar-subtree-toggle)
         ("<down-mouse-1>" . dired-sidebar-mouse-subtree-cycle-or-find-file))
  :general
  (fb/leader-keys
    "tt" '(fb/dired-sidebar-toggle :which-key "file tree")))

(use-package dhall-mode
  :mode "\\.dhall\\'")

(require 'project)
(defun project-find-go-module (dir)
  "Set the project root. DIR directory to search for project"
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Set the project root. PROJECT project to handle"
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package ledger-mode
  :mode "\\.journal\\'"
  :config
  (setq ledger-reconcile-default-commodity "€"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package org
  :init
  (setq org-default-notes-file "~/org/inbox.org")
  :config
  ;; Colour code priorities
  (setq org-priority-faces
    '((?A :foreground "#ff6c6b" :weight bold)
    (?B :foreground "#98be65")
    (?C :foreground "#51afef")))

  ;; Define energy levels
  (setq org-global-properties
    '(("ENERGY_ALL" . "Low Medium High")))

  ;; Colour code task status
  (setq org-todo-keyword-faces
    '(("TODO" :foreground "#51afef" :weight bold)
    ("DONE" :foreground "#98be65" :weight bold)
    ("WAIT" :foreground "#da8548")))

  (setq org-agenda-custom-commands
    '(("x" "Overview"
        ((tags-todo "PRIORITY=\"A\"+ENERGY=\"Low\""
            ((org-agenda-overriding-header "Quick Wins (High Impact, Low Energy)")))
        (tags-todo "PRIORITY=\"A\"+ENERGY=\"High\""
            ((org-agenda-overriding-header "Deep Work (Focus Required)")))
        (tags-todo "PRIORITY=\"A\"+ENERGY=\"Medium\""
            ((org-agenda-overriding-header "High Priority (Medium Energy)"))))))))

;; Bound outside `use-package org' so it exists before org is loaded.
(fb/leader-keys
  "c c"  '(org-capture :which-key "org-capture"))

(setq org-capture-templates
    '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
        "* TODO [#%^{Priority|A|B|C}] %^{Task Name} %^g\nDEADLINE: %^t\n:PROPERTIES:\n:ENERGY: %^{Energy?|Low|Medium|High}\n:END:\n")

    ("i" "Idea/Note" entry (file+headline "~/org/inbox.org" "Notes")
        "* %?\n%U\n")

    ("p" "Project Task" entry (file+headline "~/org/projects.org" "Projects")
        "* TODO [#%^{Priority|A|B|C}] %^{Task Name} [/]\nDEADLINE: %^t\n:PROPERTIES:\n:ENERGY: %^{Energy?|Low|Medium|High}\n:END:\n")

    ;; 5-person daily stand-up capture template
    ;; Put this inside your org-capture-templates list
    ("s" "Stand-up" entry (file+headline "~/org/standups.org" "Standups")
     "* %<%Y-%m-%d %A>
:PROPERTIES:
:Date: %T
:END:
** %?

** Action points

** Notes
"
     :empty-lines 1 :kill-buffer t)
    ))

(use-package org-download)
(use-package org-roam)
(use-package org-roam-ui)
(use-package org-mime)
(use-package org-pomodoro)

(use-package protobuf-mode)

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

(use-package systemd)

(use-package puppet-mode
  :defer t
  :mode "\\.puppet\\'")

;; (use-package nov
;;   :defer t
;;   :mode "\\.epub\\'")

(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.tmpl\\'")

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package js2-mode
  :mode "\\.js\\'")

;; Auto-detect the active virtualenv per buffer (uv .venv, poetry, direnv, …)
;; and point python-shell/eglot/ruff/apheleia at that venv's executables.
;; Requires the `dasel` binary (added to home.packages).
(use-package pet
  :ensure t
  :init
  ;; depth -10 so the venv is resolved before other python-base-mode hooks run
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; ruff lint diagnostics. Eglot owns flymake in managed buffers, so register
;; ruff's backend after eglot takes over to show pyright + ruff together.
(use-package flymake-ruff
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-base-mode)
                (flymake-ruff-load)))))

;; Async format-on-save for Python via ruff (import sort + format). pet makes
;; the project-local ruff the one apheleia runs.
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  :hook ((python-base-mode . apheleia-mode)))

;; Buffer-local direnv environments, so eglot finds per-project (nix devshell)
;; tools. envrc's docs ask for this to be enabled last.
(use-package envrc
  :demand t
  :config
  (envrc-global-mode))

(provide 'base-init)
;;; base-init.el ends here

(provide 'init)
;;; init.el ends here
