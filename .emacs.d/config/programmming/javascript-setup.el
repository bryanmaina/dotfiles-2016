(require 'json)
(require 'flycheck)

(use-package js2-mode :ensure t :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.json\\'" . js2-mode))
  :commands js2-mode
  :init (progn
          (setq-default js2-basic-offset 2
                        js2-indent-switch-body t
                        js2-auto-indent-p t
                        js2-global-externs '("angular")
                        js2-indent-on-enter-key t
                        flycheck-disabled-checkers '(javascript-jshint)
                        flycheck-checkers '(javascript-eslint)
                        flycheck-eslintrc "~/.eslintrc"))
          (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
          ;; (add-to-list 'js2-mode-hook 'flycheck-mode)
	  )

(defun my-parse-jslinter-warning (warning)
  (flycheck-error-new
   :line (1+ (cdr (assoc 'line warning)))
   :column (1+ (cdr (assoc 'column warning)))
   :message (cdr (assoc 'message warning))
   :level 'error
   :buffer (current-buffer)
   :checker 'javascript-jslinter))
(defun jslinter-error-parser (output checker buffer)
  (mapcar 'parse-jslinter-warning
          (cdr (assoc 'warnings (aref (json-read-from-string output) 0)))))
(flycheck-define-checker javascript-jslinter
  "A JavaScript syntax and style checker based on JSLinter.

See URL `https://github.com/tensor5/JSLinter'."
  :command ("c:/Users/Felix/AppData/Roaming/npm/jslint" "--raw" source)
  :error-parser jslinter-error-parser
  :modes (js-mode js2-mode js3-mode))

(use-package company-tern
  :ensure
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'company '(add-to-list 'company-backends 'company-tern)))
