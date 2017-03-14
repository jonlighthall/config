(setq exec-path '("C:/cygwin/bin/"))
(setq-default ispell-program-name "aspell")

(setenv "CYGWIN" "nodosfilewarning")

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "C")

;;(require 'diff)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

;;(add-hook 'flyspell-mode-hook 'flyspell-buffer) ;; slows down loading

;; Set frame size
(set-frame-size (selected-frame) 99 39);; (columns,rows)
;; "snap" width is 73 (for 1280 px wide display)
;; "pretty-print" width is 97 columns wide (+2 for line numbers)
;; max height with triple-hieght taskbar is 40 (1366 res.)

;; Set frame position
(setq initial-frame-alist '((top . 0) (left . 0)))
;; moves window to upper left corner

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

(custom-set-variables
 ;; Split ediff window vertically
 '(ediff-split-window-function (quote split-window-horizontally))
 ;; Turn off welcome page
 '(inhibit-startup-screen t))

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
;;(setq linum-format "%d ")

;; Custom keyboard shortcuts
(global-set-key (kbd "C-x e") 'ediff-buffers) 
(global-set-key (kbd "C-x w") 'ediff-revision)  

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
