;; color theme
(use-package color-theme
  :ensure t
  :config
  (color-theme-initialize)
;;  (color-theme-hober))
  (color-theme-andreas))

(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

(add-hook 'after-make-frame-functions
	  (lambda (frame)
  (with-selected-frame frame
    (set-cursor-color "black"))))
