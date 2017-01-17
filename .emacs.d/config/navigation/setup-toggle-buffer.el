;;; Toggle between buffers
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))
;; (other-buffer &optional BUFFER VISIBLE-OK FRAME)
;; - Return most recently selected buffer other than BUFFER. Ignore the argument
;;   BUFFER unless it denotes a live buffer.
;; - If VISIBLE-OK==1, a buffer is returned even when it is visible in a split
;;   window.Buffers not visible in windows are preferred to visible buffers,
;;   unless optional second argument VISIBLE-OK is non-nil.
;; - If the optional third argument FRAME is non-nil, use that frame's buffer
;;   list instead of the selected frame's buffer list.

;;; One Window Toggle
(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window' function is called.")

(defvar modi/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `modi/toggle-one-window' function was called.")

(defun modi/toggle-one-window (&optional force-one-window)
  "Toggle the frame state between deleting all windows other than the current window and the windows state prior to that."
  (interactive "P")
  (if (or (null (one-window-p))
          force-one-window)
      (progn
        (setq modi/toggle-one-window--buffer-name (buffer-name))
        (setq modi/toggle-one-window--window-configuration (current-window-configuration))
        (delete-other-windows))
    (progn
      (when modi/toggle-one-window--buffer-name
        (set-window-configuration modi/toggle-one-window--window-configuration)
        (switch-to-buffer modi/toggle-one-window--buffer-name)))))

(bind-keys
  ("C-x 1"        . modi/toggle-one-window) ; default binding to `delete-other-windows'
  ("C-("          . toggle-between-buffers)
  ("C-c ("        . toggle-between-buffers) ; alternative to C-( for terminal mode
  )

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "ZZ" #'toggle-between-buffers))

;; managing buffers
(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
	 ;; ("C-x b" . helm-buffers-list)
	 ("C-x r b" . helm-bookmarks)
	 ("M-y" . helm-show-kill-ring)))
