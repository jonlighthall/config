;; ------Universal Emacs settings--------------------------
;; The following settings are intended to be system independent

;; Set line width for fortran and other
(set 'fcols 72)
(set 'ncols 97)

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(inhibit-startup-screen t)
 '(visible-bell t)
 '(fortran-line-length fcols)
 '(fortran-continuation-string "&"))

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; Turn on line numbering
(require 'linum)
(global-linum-mode 1)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; ------Custom keyboard shortcuts-------------------------
(global-set-key (kbd "C-x e") 'ediff-buffers)
(global-set-key (kbd "C-x w") 'ediff-revision)
(global-set-key (kbd "C-x d") 'ediff-current-file)
(global-set-key (kbd "C-8")
                (lambda () (interactive)
                  (load-theme 'misterioso t)))
(global-set-key (kbd "C-9")
		(lambda () (interactive)
                  (disable-theme 'misterioso)))
(defun select-all-and-indent ()
  "mark whole buffer and indent region"
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace)
  (goto-char 1)
  (delete-blank-lines) ;; should only delete repeated blanks
  )
(global-set-key (kbd "C-x j") 'select-all-and-indent)

(defun select-all-and-untabify ()
  "mark whole buffer and untabify"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-x t") 'select-all-and-untabify)

;; sort .bash_history file by timestamp
(defun sort-bash-history ()
  "sort bash history"
  (interactive)
  ;; clean up white space
  (delete-trailing-whitespace)
  (flush-lines "^$" (point-min) (point-max)) ; delete all empty lines

  ;; collapse all lines to their corresponding timestamps
  (goto-char 1)
  (replace-regexp "^#+[0-9]\\{10\\}.*$" "\\&$$$") ; find timestamp lines
  (goto-char 1)
  (replace-regexp "\\$\\$\\$\n#" "\n#") ; ignore repeated timestamps
  (goto-char 1)
  (replace-regexp "\\$\\$\\$\n" "$$$") ; merge commands with timestamps
  (goto-char 1)
  (replace-regexp "\n[^#].*$" "@@@\\&") ; find all orphaned lines
  (goto-char 1)
  (replace-regexp "@@@\n" ";") ; merge orphaned lines
  (goto-char 1)
  (replace-regexp "^#+[^0-9].*$" "@@@\\&")
  (goto-char 1)
  (replace-regexp "\n@@@" ";")

  ;; uniquify and sort
  (delete-duplicate-lines (point-min) (point-max))
  (sort-lines nil (point-min) (point-max))

  ;; unmerge commands with timestamps
  (goto-char 1)
  (replace-regexp "\\$\\$\\$" "\n")

  ;; clean up quotes
  (goto-char 1)
  (replace-regexp "^[^\n\"]*\"[^\n\"]*$" "\\&;\" # unmatched quote")

  (goto-char 1)
  (replace-regexp "^[^\n`]*`[^\n`]*$" "\\&;` # unmatched grave")

  (goto-char 1)
;;(replace-regexp "^[^\n'\"\\]*'[^\n'\"]*$" "\\&;' # unmatched apostrophe")
  (replace-regexp "^[^\n'\"`]*'[^\n'\"`]*$" "\\&;' # unmatched apostrophe")
;;(replace-regexp "(?!^.*\".*'+.*\".*$)(?!^.*`.*'+.*`.*$)^[^\n']*(?<!\\)'[^\n']*$" "\\&;' # unmatched apostrophe")

  (deactivate-mark)
  (goto-char 1)
  (prin1 "done sorting history")
  )
(global-set-key (kbd "C-x y") 'sort-bash-history)

;; start Git merge conflicts in smerge ediff
(defun vc-git-find-file-hook ()
  (when (save-excursion
	  (goto-char (point-min))
	  (re-search-forward "^<<<<<<< " nil t))
    (smerge-ediff)))

;; setup files ending in “.m” to open in octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; open arduino as c++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; setup files containing “makefile” to open in makefile-mode
(add-to-list 'auto-mode-alist '("[Mm]akefile.+\\'" . makefile-mode))

;; files containing “bash” to open in shell-script-mode
(add-to-list 'auto-mode-alist '(".bash.+\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))
(add-to-list 'auto-mode-alist '("\\.cmn\\'" . fortran-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))

;; turn off starup message
(defun display-startup-echo-area-message ()
  (message nil))

;; set no-window (terminal) colors
(when (not window-system)
  ;;(setq frame-background-mode 'light)
  (add-to-list 'default-frame-alist '(foreground-color . "#000")) ; black
  (add-to-list 'default-frame-alist '(background-color . "#FFF")) ; white

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(minibuffer-prompt ((t (:background "#FFF"  :foreground "medium blue")))))
  )

;; whitespace-mode settings
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column ncols)
(setq-default fill-column ncols)
(global-whitespace-mode 1)
;;(add-hook 'prog-mode-hook 'whitespace-mode)

;; add bullets to fill
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

;; FORTRAN column highlighting
(add-hook 'fortran-mode-hook 'turn-on-auto-fill)
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (setq-local whitespace-line-column fcols)))
(add-hook 'fortran-mode-hook
	  (lambda ()
	    (setq-local global-whitespace-mode 1)))
