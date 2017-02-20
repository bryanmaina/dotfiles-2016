(use-package emmet-mode
  :ensure t)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq-default flycheck-disabled-checkers '(html-tidy))
