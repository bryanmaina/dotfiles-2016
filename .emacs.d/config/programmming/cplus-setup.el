;;; Source code navigation using RTags
(use-package company
  :ensure t)

(require 'company-rtags)

(use-package rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(setq rtags-use-helm t)


;;; Source code completion using Irony

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Using Company with Irony

(use-package company-irony
  :ensure t
  :config
  (setq company-idle-delay 0)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends)))

(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

;;; Header file completion with company-irony-c-headers

(use-package company-irony-c-headers
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))

;;; Syntax checking with Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (progn
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

;; Integrating RTags with Flycheck

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; Rtags creates more accurate overlays
  (setq-local flycheck-check-syntax-automatically nil))

;; c-mode-common-hook is also called by C++-mode
(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)

(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;; CMake automation with cmake-ide
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

;;; To have cmake-ide automatically create a
;; compilation commands file in your project
;; root create a .dir-locals.el containing the following:
;;   ((nil . ((cmake-ide-build-dir . "<PATH_TO_PROJECT_BUILD_DIRECTORY>"))))
