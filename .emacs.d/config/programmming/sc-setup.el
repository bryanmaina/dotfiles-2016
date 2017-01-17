(require 'sclang)

(use-package sclang-extensions
  :ensure t
  :config
  (add-hook 'sclang-mode-hook 'sclang-extensions-mode))

(use-package sclang-snippets
  :ensure t)
