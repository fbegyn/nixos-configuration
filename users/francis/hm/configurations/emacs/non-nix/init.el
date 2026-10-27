;;; init --- Standalone (non-nix) init -*- lexical-binding: t; -*-
;;; Commentary:
;; Bootstraps packages from MELPA/GNU ELPA (and package-vc for majutsu), then
;; loads the shared ../base-init.el.  Needs Emacs 30+ (package-vc).
;;; Code:
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Packages base-init.el needs before use-package runs.
(dolist (p '(use-package general diminish))
  (unless (package-installed-p p)
    (package-install p)))

;; Everything `use-package' mentions gets installed (nix does this with
;; alwaysEnsure); built-ins are marked `:ensure nil' in base-init.el.
(setq use-package-always-ensure t)

;; majutsu is not on MELPA: install from git like the nix build does.
(unless (package-installed-p 'majutsu)
  (when (fboundp 'package-vc-install)
    (condition-case err
        (package-vc-install '(majutsu :url "https://github.com/0WD0/majutsu"
                                      :branch "v0.6.0"))
      (error (message "majutsu install failed: %S" err)))))

;; Tree-sitter grammars: nix ships them, here run
;; M-x fb/nn-install-grammars once.
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")))

(defun fb/nn-install-grammars ()
  "Build and install all configured tree-sitter grammars that are missing."
  (interactive)
  (dolist (entry treesit-language-source-alist)
    (unless (treesit-language-available-p (car entry))
      (treesit-install-language-grammar (car entry)))))

;; Shared config.
(load (expand-file-name "../base-init.el" fb/nn-dir) nil nil t)
(when (file-exists-p custom-file) (load custom-file nil t))

;;; init.el ends here
