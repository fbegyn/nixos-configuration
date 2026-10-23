;;; early-init --- My emacs early init file
;;; Commentary:
;;; Code:
;; Make GC rare during init only. After startup `gcmh' (see base-init.el)
;; owns the threshold, so don't also fiddle with it from minibuffer hooks.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; Nix manages our packages
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; Native compilation: compile lazily in the background and don't nag.
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-jit-compilation t)
(setopt native-comp-async-on-battery-power nil)

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
