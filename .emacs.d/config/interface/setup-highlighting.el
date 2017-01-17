;cur;; Show Paren
;; Allow one to see matching pairs of parentheses
;; When point is on one of the paired characters, highlight the other
(show-paren-mode 1)

;; Highlights changes to the buffer caused by commands
;; such as ‘undo’, ‘yank’/’yank-pop’, etc.
(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; highlight-symbol settings
(use-package highlight-symbol :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "#FA009A")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

;; Highlight indentation
;;(use-package highlight-indent-guides
;;  :ensure t
;;  :init (setq highlight-indent-guides-method 'fill
;;	      highlight-indent-guides-auto-enabled nil)
;;  :config
;;  (set-face-background 'highlight-indent-guides-odd-face "aquamarine1")
;;  (set-face-background 'highlight-indent-guides-even-face "pink1")
;;  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
