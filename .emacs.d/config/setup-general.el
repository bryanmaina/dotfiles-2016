(use-package nlinum
  :ensure t)

(defun initialize-nlinum (&optional frame)
  (require 'nlinum)
  (add-hook 'prog-mode-hook 'nlinum-mode))
(when (daemonp)
  (add-hook 'window-setup-hook 'initialize-nlinum)
  (defadvice make-frame (around toggle-nlinum-mode compile activate)
    (nlinum-mode -1) ad-do-it (nlinum-mode 1)))

;; Track servers for Emacs daemon
(require 'server)
(unless (server-running-p)
  (server-start))

;; Schema validation for Emacs-lisp
(use-package validate
  :ensure t)

;; Try to remember last cursor position
(use-package saveplace
  :ensure t
  :config
  (setq server-visit-hook (quote (save-place-find-file-hook))))

;; terminal mouse support
;; make mouse selection to be emacs region making
(require 'mouse)
(xterm-mouse-mode t)
