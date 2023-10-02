;; ------Settings for Linux -------------------------------
(load "${HOME}/config/emacs_all.el")

(when (not window-system)
  (message "Emacs not running in window.")

  (if (<= emacs-major-version 23) 
      (progn 
	(message "defining mouse scroll...")
	(global-set-key [mouse-4] 'scroll-down)
	(global-set-key [mouse-5] 'scroll-up)
	)
    (message "Emacs version supports scroll lines.")
    (when (fboundp 'scroll-down-line)
	(message "defining mouse scroll lines...")
      (global-set-key [mouse-4] 'scroll-down-line)
      (global-set-key [mouse-5] 'scroll-up-line)
      )
    )
  )

;; default to unified diffs
;(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; ------Frame appearance and behavior---------------------
;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; window (non-terminal) setings
(when window-system-version
  ;; Set frame position
  ;; intended to move window to upper left corner
  (setq initial-frame-alist '((top . 70) (left . 0))) 
  
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

;; (load-file "${HOME}/.emacs.d/setnu.el")
;; (load-file "${HOME}/.emacs.d/psvn.el")
