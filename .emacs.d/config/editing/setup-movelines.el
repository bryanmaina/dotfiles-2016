(use-package move-text
  :ensure t)

(global-set-key
 (kbd "C-c m")
 (defhydra hydra-move-text ()
   "Move text"
   ("s" move-text-up "up")
   ("w" move-text-down "down")))
