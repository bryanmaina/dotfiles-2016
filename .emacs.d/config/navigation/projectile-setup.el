(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-find-dir)
    (setq projectile-find-dir-includes-top-level t)))
