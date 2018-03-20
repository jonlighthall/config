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

;; Set frame position
(setq initial-frame-alist '((top . 70) (left . 0))) ; moves window to upper left corner

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;may not work in X-windows

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

;;; ------Custom keyboard shortcuts-------------------------
(global-set-key (kbd "C-x e") 'ediff-buffers) 
(global-set-key (kbd "C-x w") 'ediff-revision)
(global-set-key (kbd "C-x d") 'ediff-current-file)  

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun vc-git-find-file-hook ()
  (when (save-excursion
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t))
    (smerge-start-session)))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
