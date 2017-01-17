(use-package eclim
  :ensure t
  :config
  (setq eclimd-autostart t)
  (setq eclim-auto-save t)
  (setq eclimd-default-workspace "/home/dumy/Projects/Learning_Java/")
  (global-eclim-mode)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(use-package company-emacs-eclim
  :ensure t
  :config
  (company-emacs-eclim-setup)
  (global-company-mode t))
