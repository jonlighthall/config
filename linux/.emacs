;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; default to unified diffs
;(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;;; ------Frame appearance and behavior---------------------
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when window-system-version
  ;; setup for graphic environment

  ;; Set frame position
  (setq initial-frame-alist '((top . 70) (left . 0))) ; moves window to upper left corner
  
  ;; Window transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;may not work in X-windows

  ;; Fonts
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )

(custom-set-variables
 ;; Split ediff window vertically
 '(ediff-split-window-function (quote split-window-horizontally))
 ;; Turn off welcome page
 '(inhibit-startup-screen t))

;;; ------Highlighting and coding aides---------------------
;; turn on font-lock mode
(global-font-lock-mode t)

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)
;; (load-file "~/.emacs.d/setnu.el")
;; (load-file "~/.emacs.d/psvn.el")

;; FORTRAN column highlighting
(custom-set-variables
 '(fortran-line-length 72)
 '(fortran-continuation-string "&"))

(require 'whitespace)
(setq whitespace-style '(lines-tail))
(setq whitespace-line-column 80)
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (setq whitespace-line-column 72)))
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (global-whitespace-mode 1)))
(add-hook 'fortran-mode-hook 'turn-on-auto-fill)

;;(add-hook 'prog-mode-hook 'whitespace-mode)

(load "~/config/emacs_all.el")
