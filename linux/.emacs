;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
;(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; Turn off welcome page
(custom-set-variables '(inhibit-startup-screen t))

;; turn on font-lock mode
(global-font-lock-mode t)

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)

;; (load-file "~/.emacs.d/setnu.el")
;; (load-file "~/.emacs.d/psvn.el")
