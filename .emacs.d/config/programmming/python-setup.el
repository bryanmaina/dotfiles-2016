(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi"))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Python Hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq-default indent-tabs-mode t)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

(use-package python
  :ensure t
  :config
  (progn
    (when (executable-find "flake8")
      (setq python-check-command "flake8"))
    (add-hook 'python-mode-hook
	      (lambda ()
		(setq electric-indent-chars '(?\n))))))


(use-package "pyvenv"
  :ensure t
  :config
  (defalias 'workon 'pyvenv-workon)
  (pyvenv-mode))
