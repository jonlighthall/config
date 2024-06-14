;; ------Settings for DOS and Cygwin-----------------------

;; ------Libraries-----------------------------------------
(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;;(require 'diff)

(load "c:/Users/jlighthall/config/emacs_all.el")

;; ------Path----------------------------------------------
(setq exec-path '(
		  "C:/cygwin/bin/"
		  "C:/Users/jlighthall/AppData/Local/Programs/Git/cmd/"
		  "C:/Users/jlighthall/AppData/Local/Programs/Git/bin/"
		  ))

;; ------DOS-----------------------------------------------
(setenv "CYGWIN" "nodosfilewarning")

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "C")

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; ------Spelling------------------------------------------
;;(setq-default ispell-program-name "aspell")
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

;; ------Frame appearance and behavior---------------------
;; Set frame position
(setq initial-frame-alist '((top . 10) (left . 0))) ; moves window to upper left corner

;; Set frame size
(set-frame-size (selected-frame) 154 80); (columns,rows)
					; "snap" width is 73 (for 1280 px wide display)
					; "pretty-print" width is 97 columns wide (+2 for line numbers)
					; max height with triple-hieght taskbar is 40 (1366 res.)

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; ------Highlighting and coding aides---------------------
;; Syntax highlighting (font-lock-mode)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))
