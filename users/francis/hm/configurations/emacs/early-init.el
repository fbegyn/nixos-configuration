;;; early-init --- My emacs early init file
;;; Commentary:
;;; Code:
(defun hm/reduce-gc ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun hm/restore-gc ()
  "Restore the frequency of garbage collection."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Make GC more rare during init, while minibuffer is active, and
;; when shutting down. In the latter two cases we try doing the
;; reduction early in the hook.
(hm/reduce-gc)
(add-hook 'minibuffer-setup-hook #'hm/reduce-gc -50)
(add-hook 'kill-emacs-hook #'hm/reduce-gc -50)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'hm/restore-gc)
(add-hook 'minibuffer-exit-hook #'hm/restore-gc)

;; Nix manages our packages
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; Native compilation: compile lazily in the background and don't nag.
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-jit-compilation t)

;; Avoid GC stalls when rendering icon/ligature fonts.
(setq inhibit-compacting-font-caches t)

;; Avoid expensive frame resizing. Inspired by Doom Emacs.
(setq frame-inhibit-implied-resize t)

;; Remove all UI features
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Set the font early to avoid a first-frame resize flash.
(push (cons 'font (if (eq system-type 'darwin) "Menlo 14" "DejaVu Sans Mono 14"))
      default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
