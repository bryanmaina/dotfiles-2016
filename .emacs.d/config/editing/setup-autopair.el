(use-package autopair
  :ensure t)

(defvar autopair-modes '(r-mode
			 ruby-mode
			 lisp-mode
			 java-mode
			 emacs-lisp-mode
			 cider-mode
			 org-mode
			 clojure-mode
			 python-mode))

(defun turn-on-autopair-mode ()
  (autopair-mode 1))

(dolist (mode autopair-modes)(add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

(use-package paredit
  :ensure t)

(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on"
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)
    ))

(ad-activate 'paredit-mode)