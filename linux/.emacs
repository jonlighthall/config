;; ------Settings for Linux -------------------------------
(load "~/config/emacs_all.el")

;; default to unified diffs
;(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;;; ------Frame appearance and behavior---------------------
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

;;; ------Highlighting and coding aides---------------------
;; turn on font-lock mode
(global-font-lock-mode t)

;; (load-file "~/.emacs.d/setnu.el")
;; (load-file "~/.emacs.d/psvn.el")
