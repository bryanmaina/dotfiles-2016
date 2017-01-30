(use-package multiple-cursors
  :config
  ;; multiple-cursors
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  (global-set-key (kbd "C-c q r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c q c") 'mc/edit-lines)
  (global-set-key (kbd "C-c q e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c q a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-x q") 'dired-toggle-read-only))
