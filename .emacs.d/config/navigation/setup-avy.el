;; jump to a character or a line
(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'at-full)
  (setq avy-background t)
  :bind* (("M-s" . avy-goto-char-timer)
          ("M-m" . avy-goto-line)))
