;;; ------Custom keyboard shortcuts-------------------------
(global-set-key (kbd "C-x e") 'ediff-buffers) 
(global-set-key (kbd "C-x w") 'ediff-revision)
(global-set-key (kbd "C-x d") 'ediff-current-file)
(global-set-key (kbd "C-8")
                (lambda () (interactive)
                  (load-theme 'misterioso t)))
(global-set-key (kbd "C-9")
		(lambda () (interactive)
                  (disable-theme 'misterioso)))

(defun vc-git-find-file-hook ()
  (when (save-excursion
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t))
    (smerge-start-session)))

;; setup files ending in “.m” to open in octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; setup files containing “makefile” to open in makefile-mode
(add-to-list 'auto-mode-alist '("[Mm]akefile.+\\'" . makefile-mode))
