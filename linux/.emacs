;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; default to unified diffs
;(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;;; Frame appearance and behavior
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Window transparency, may not work in X-windows
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))

(custom-set-variables
 ;; Split ediff window vertically
 '(ediff-split-window-function (quote split-window-horizontally))
 ;; Turn off welcome page
 '(inhibit-startup-screen t))

;;; Highlighting and coding aides
;; turn on font-lock mode
(global-font-lock-mode t)

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; (load-file "~/.emacs.d/setnu.el")
;; (load-file "~/.emacs.d/psvn.el")

;; Custom keyboard shortcuts
(global-set-key (kbd "C-x e") 'ediff-buffers) 
(global-set-key (kbd "C-x w") 'ediff-revision)  
