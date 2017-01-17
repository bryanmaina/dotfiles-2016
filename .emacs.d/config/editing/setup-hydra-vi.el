(use-package hydra
  :ensure t)

(global-set-key
 (kbd "C-z")
 (defhydra hrya-vi ()
   "vi"
   ("l" forward-char)
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("w" forward-word)
   ("b" backward-word)
   ("W" forward-symbol)
   ("B" (lambda ()
	  (interactive)
	  (forward-symbol -1)))
   ("g" beginning-of-buffer "top")
   ("G" end-of-buffer "bottom")
   ("SPC" set-mark-command)))
