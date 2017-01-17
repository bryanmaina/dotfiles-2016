(display-time-mode 1)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-helm-mode t)
  (spaceline-info-mode))

(use-package micgoline
   :ensure t)

(setq powerline-default-separator 'alternate)
