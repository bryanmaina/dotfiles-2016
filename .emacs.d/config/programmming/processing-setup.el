(setq compilation-ask-about-save nil)

(use-package processing-snippets
  :ensure t)

(use-package processing-mode
  :ensure t
  :config
  (autoload 'processing-mode "processing-mode" "Processing mode" t)
  (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
  (autoload 'processing-snippets-initialize "processing-snippets" nil nil nil)
  (eval-after-load 'yasnippet '(processing-snippets-initialize))
  (setq processing-location "/usr/bin/processing-java")
  (setq processing-application-dir "/usr/share/applications/processing.desktop")
  (setq processing-sketchbook-dir "/home/dumy/Projects/Processing/Main/"))
