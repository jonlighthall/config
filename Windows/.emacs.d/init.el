(setq exec-path '("C:/cygwin/bin/"))
(setq-default ispell-program-name "aspell")

(setenv "CYGWIN" "nodosfilewarning")

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

;; Syntax highlighting (font-lock-mode)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; Set frame size
(set-frame-size (selected-frame) 99 40);; (columns,rows)
;; "snap" width is 73 (for 1280 px wide display)
;; "pretty-print" width is 97 columns wide (+2 for line numbers)
;; max height with triple-hieght taskbar is 40 (1366 res.)

;; Set frame position
(setq initial-frame-alist '((top . 0) (left . 0)))
;; moves window to upper left corner

;; Turn off welcome page
(custom-set-variables '(inhibit-startup-screen t))

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)
;;(setq linum-format "%d ")

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;;(require 'diff)

;; hello world 
