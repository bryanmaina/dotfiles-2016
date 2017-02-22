(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (add-hook 'json-mode-hook (lambda ()
                               (setq flycheck-checker 'json-jsonlint))))

