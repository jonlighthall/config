;; ------Universal Emacs settings--------------------------
;; The following settings are intended to be system independent

;; MF Mouse
(xterm-mouse-mode 1)
;; After enabling xterm mouse mode, normal copy and paste functions will not
;; work. Bindings are adjusted as follows
;; --------+-------------+---------------------------
;; command | before      | after
;; --------+-------------+---------------------------
;; copy    | select with | hold shift and select with
;;         | mouse and   | mouse, release shift and
;;         | hit Enter   | hit Enter
;; --------+-------------+---------------------------
;; paste   | right-click | shift-right-click
;; --------+-------------+---------------------------

;; Set line width for fortran and other
(set 'fcols 72)
(set 'ncols 97)

;; Tab settings
;; MATLAB tab size 
(set 'mtab 4)
;; general/global tab size
(set 'gtab 2)
(setq-default tab-width gtab)
;; use spaces, not tabs
(setq-default indent-tabs-mode nil)

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fortran-continuation-string ">")
 '(fortran-line-length fcols)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(visible-bell t)
 )

;; Turn on highlight matching parentheses
(show-paren-mode 1)

;; ------Libraries-----------------------------------------
;; Turn on line numbering

;;(global-linum-mode 1)
(setq global-display-line-number t)
;; whitespace-mode settings
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column ncols)
(setq-default fill-column ncols)
(global-whitespace-mode 1)
;;(add-hook 'prog-mode-hook 'whitespace-mode)

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

;; ------Custom functions----------------------------------
(defun select-all-and-indent ()
  "mark whole buffer and indent region, delete trailing whitespace, delete repeated blank lines"
  (interactive "*")
  (push-mark) ; save position
  (goto-char 1)
  (replace-regexp "^\s*#}#" "}##") ; un-comment dummy brackets
  (pop-mark) ; pop mark position saved by regexp
  (indent-region (point-min) (point-max)) ; select all
  (delete-trailing-whitespace)
  (goto-char 1)
  (replace-regexp "^\s*\n\\{2,\\}" "\n") ; delete repeated blank lines
  (pop-mark) ; pop mark position saved by regexp
  (goto-char 1)
  (replace-regexp "}##" "#}#") ; comment dummy brackets
  (pop-mark) ; pop mark position saved by regexp

  (goto-char (mark-marker)) ; go back to starting position
  (prin1 "done indenting")
  )
(global-set-key (kbd "C-x j") 'select-all-and-indent)

(defun select-all-and-untabify ()
  "mark whole buffer and untabify"
  (interactive "*")
  (push-mark)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (goto-char (mark-marker))
  (prin1 "done untabifying")
  )
(global-set-key (kbd "C-x t") 'select-all-and-untabify)

;; sort .bash_history file by timestamp
(defun sort-bash-history ()
  "sort bash history"
  (interactive "*")
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

  ;; find first timestamp
  (goto-char 1)
  (search-forward-regexp "^#[0-9]\\{10\\}" nil t)
  (move-beginning-of-line 1)
  (push-mark)

  (goto-char (mark-marker))
  (replace-regexp "\n[^#].*$" "@@@\\&") ; find all orphaned lines
  (goto-char (mark-marker))
  (replace-regexp "@@@\n" ";") ; merge orphaned lines
  (goto-char (mark-marker))
  (replace-regexp "^#+[^0-9].*$" "@&@&\\&") ; find all remaining non-timestamp lines
  (goto-char (mark-marker))
  (replace-regexp "\n@&@&" ";") ;; merge non-timestamp lines

  ;; uniquify and sort
  (delete-duplicate-lines (mark-marker) (point-max))
  (sort-lines nil (mark-marker) (point-max))

  ;; unmerge commands with timestamps
  (goto-char (mark-marker))
  (replace-regexp "\\$\\$\\$" "\n")

  ;; clean up quotes
  (goto-char (mark-marker))
  (replace-regexp "^[^\n\"]*\"[^\n\"]*$" "\\&;\" # unmatched quote EMACS")

  (goto-char (mark-marker))
  (replace-regexp "^[^\n`]*`[^\n`]*$" "\\&;` # unmatched grave EMACS")

  (goto-char (mark-marker))
  (replace-regexp "^[^\n'\"`]*\'[^\n'\"`]*$" "\\&;\' # escaped apostrophe EMACS")
  (replace-regexp "^[^\n'\"`]*'[^\n'\"`]*$" "\\&;' # unmatched apostrophe EMACS")

  (goto-char (mark-marker))
  (replace-regexp "^[^\n\"]*<<[^\n<<EOF\"]*$" "\\& <<-EOF # unmatched redirect EMACS\nEOF")

  (goto-char (mark-marker))
  (replace-regexp ";EOF" "\nEOF")

  (deactivate-mark)
  (goto-char 1)
  (prin1 "done sorting history")
  )
(global-set-key (kbd "C-x y") 'sort-bash-history)

;; sort words in region
(defun sort-words (beg end)
  "Sort words in region alphabetically.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*r")
  (sort-regexp-fields nil "\\(\\sw_\\.\\|\\s_\\|\\w\\)+" "\\&" beg end))

;; start Git merge conflicts in smerge ediff
(defun vc-git-find-file-hook ()
  (save-excursion
    (goto-char (point-min))
	(when (re-search-forward "^<<<<<<< " nil t)
      (smerge-ediff))))
(add-hook 'find-file-hook 'vc-git-find-file-hook t)

;;https://emacs.stackexchange.com/questions/3074/customizing-indentation-in-makefile-mode
;;Building on purple_arrows' solution:

(defun my-makefile-indent-line ()
  (save-excursion
    (forward-line 0)
    (cond
     ;; keep TABs
     ((looking-at "\t")
      t)
     ;; indent continuation lines to 4
     ((and (not (bobp))
	   (= (char-before (1- (point))) ?\\))
      (delete-horizontal-space)
      (indent-to 4))
     ;; delete all other leading whitespace
     ((looking-at "\\s-+")
      (replace-match "")))))

(add-hook 'makefile-mode-hook
	  (lambda ()
	    (setq-local indent-line-function 'my-makefile-indent-line)))

;; MATLAB
;; ------
;; setup files ending in “.m” to open in octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; tabbing
(add-hook 'octave-mode-hook
	  (lambda ()
	    ;; don't indent every line for no reason
	    (setq indent-line-function 'insert-tab)
	    )
	  )

(setq octave-mode-hook
      (lambda ()
	(progn
	  (setq indent-tabs-mode t)                   
          (setq tab-width mtab)
          (setq tab-stop-list (number-sequence mtab 200 mtab))
	  ;; set code block indent
	  (setq octave-block-offset mtab)

	  ;; keep comments from being inappropriately indented
	  (setq octave-comment-char ?%)
          (setq comment-start "%")
	  (setq comment-add 0)	   
          (defun octave-indent-comment ()
            "A function for `smie-indent-functions' (which see)."
            (save-excursion
              (back-to-indentation)
              (cond
               ((octave-in-string-or-comment-p) nil)
               ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}") 0)
	       )
	      )
	    )
	  )
	)
      )

;; open arduino files as c++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; open makefiles in makefile-mode
(add-to-list 'auto-mode-alist '("[Mm]akefile.+\\'" . makefile-mode))

;; open bash in shell-script-mode
(add-to-list 'auto-mode-alist '(".bash.+\\'" . shell-script-mode))

;; open batch files in bat-mode
(add-to-list 'auto-mode-alist '("\\.bat\\'" . bat-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . shell-script-mode))

;; open Fortran includes and configuration managed files in fortran-mode
(add-to-list 'auto-mode-alist '("\\.cmn\\'" . fortran-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))
(add-to-list 'auto-mode-alist '("\\.unm\\'" . fortran-mode))

;; open change logs and readme in text-mode
(add-to-list 'auto-mode-alist '("read.me\\'" . text-mode))
(add-to-list 'auto-mode-alist '("README\\'" . text-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG\\'" . text-mode))

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

;; add bullets to fill
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

;; FORTRAN column highlighting
(add-hook 'fortran-mode-hook
          (lambda ()
	    'turn-on-auto-fill
            (setq-local whitespace-line-column fcols)
	    (setq-local global-whitespace-mode 1)))
