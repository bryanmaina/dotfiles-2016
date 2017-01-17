;;; Duplicate current line or region
;; http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))
	)
      (goto-char (+ origin (* (length region) arg) arg))
      )
    )
  )

(bind-keys
 ("C-S-d" . duplicate-current-line-or-region)
 ("C-M-d" . duplicate-current-line-or-region) ; alternative to C-S-d in terminal
 )
