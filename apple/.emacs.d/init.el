;; Syntax highlighting (font-lock-mode)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; Set frame size
(set-frame-size (selected-frame) 99 84);; (columns,rows)
;; "snap" width is 73 (for 1280 px wide display)
;; "pretty-print" width is 97 columns wide (+2 for line numbers)
;; max height with triple-hieght taskbar is 40 (1366 res.)

;; Set frame position
(setq initial-frame-alist '((top . 0) (left . 0)))
;; moves window to upper left corner

;; Turn off welcome page
(custom-set-variables '(inhibit-startup-screen t))

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)
(setq linum-format "  %d")

(load "~/config/emacs_all.el")
