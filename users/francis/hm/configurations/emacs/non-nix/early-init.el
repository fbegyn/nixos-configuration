;;; early-init --- Standalone (non-nix) early init -*- lexical-binding: t; -*-
;;; Commentary:
;; Used via `emacs --init-directory=<this dir>' (see ./emacs-nn).  State
;; (elpa, caches, custom file) lives in ~/.local/share/emacs-non-nix so the
;; nix-managed ~/.emacs.d and ~/.config/emacs are never touched.
;;; Code:
(defvar fb/nn-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar fb/nn-data-dir (expand-file-name "~/.local/share/emacs-non-nix/"))
(make-directory fb/nn-data-dir t)

(setq user-emacs-directory fb/nn-data-dir
      package-user-dir (expand-file-name "elpa/" fb/nn-data-dir)
      custom-file (expand-file-name "custom.el" fb/nn-data-dir))
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" fb/nn-data-dir)))

;; Share the GC/UI/font settings with the nix config.
(load (expand-file-name "../early-init.el" fb/nn-dir) nil t)
;; We call `package-initialize' ourselves in init.el.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
