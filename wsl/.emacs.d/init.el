;;; ------Settings for DOS and Cygwin-----------------------
;;(setq exec-path '("C:/cygwin/bin/"))
(setq-default ispell-program-name "aspell")

;;(setenv "CYGWIN" "nodosfilewarning")

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
;;(setenv "LANG" "C")

(require 'diff)

;; (defun remove-dos-eol ()
;;   "Do not show ^M in files containing mixed UNIX and DOS line endings."
;;   (interactive)
;;   (setq buffer-display-table (make-display-table))
;;   (aset buffer-display-table ?\^M []))

;;; ------Spelling------------------------------------------
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    ))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Program-mode spell checking for most program languages
(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
        '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook 
			     ruby-mode-hook java-mode-hook))

;;(add-hook 'flyspell-mode-hook 'flyspell-buffer) ;; slows downloading

;;; ------Frame appearance and behavior---------------------
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(when window-system-version
  ;; setup for graphic environment
  
  ;; Set frame position
  (setq initial-frame-alist '((top . 40) (left . 0))) ; moves window to upper left corner

  ;; Set frame size
  (set-frame-size (selected-frame) 101 93);; (columns,rows)
					; "snap" width is 73 (for 1280 px wide display)
					; "pretty-print" width is 97 columns wide (+2 for line no)
					; max height with triple-hieght taskbar is 40 (1366 res.)

  ;; Window transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  ;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

  ;; Fonts
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 110 :width normal)))))
  )

;;; ------Highlighting and coding aides---------------------
;; Syntax highlighting (font-lock-mode)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)

;; FORTRAN column highlighting
(custom-set-variables
 '(fortran-line-length 72)
 '(fortran-continuation-string "&"))

(require 'whitespace)
(setq whitespace-style '(lines-tail))
;;(setq whitespace-line-column 50)
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (setq-local whitespace-line-column 72)))
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (setq-local global-whitespace-mode 1)))
(add-hook 'fortran-mode-hook 'turn-on-auto-fill)

(load "~/config/emacs_all.el")
