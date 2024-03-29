;; ------Settings for WSL----------------------------------
(load "${HOME}/config/emacs_all.el")

;; ------Libraries-----------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'diff)

;; ------Spelling------------------------------------------
(setq-default ispell-program-name "aspell")

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    ))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; The following settings may slow startup

;; Program-mode spell checking for most program languages
;; (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
;;         '(c-mode-common-hook tcl-mode-hook emacs-lisp-mode-hook 
;; 			     ruby-mode-hook java-mode-hook))

;;(add-hook 'flyspell-mode-hook 'flyspell-buffer)

;; ------Frame appearance and behavior---------------------
;; window (non-terminal) setings
(when window-system
  ;; Set frame position
  ;; intended to move window to upper left corner
  (setq initial-frame-alist '((top . 197) (left . 1))) ;; Windows 11 WSLg

  ;; Set frame size
  (set-frame-size (selected-frame) 178 49);; (columns,rows)

  ;; Window transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  ;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
  )

;; ------Highlighting and coding aides---------------------
;; Syntax highlighting (font-lock-mode)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))
