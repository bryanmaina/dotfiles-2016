;;; Scrolling
;; Keep point at its screen position if the scroll command moved it vertically
;; out of the window, e.g. when scrolling by full screens using C-v.
(setq scroll-preserve-screen-position t)

;; Scroll without moving the point/cursor
(defun modi/scroll-up (ln)
  "Scroll up by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-up ln))

(defun modi/scroll-down (ln)
  "Scroll down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-down ln))

;; https://github.com/politza/pdf-tools/issues/227#issuecomment-242100968
(defun modi/scroll-other-window (ln)
  "Scroll the buffer in other window.
This command supports pdf file buffers too (`pdf-view-mode').
If LN is positive, scroll the buffer up.
If LN is negative, scroll the buffer down."
  (interactive "p")
  (let ((other-win (other-window-for-scrolling)))
    (if (and (fboundp #'pdf-util-pdf-window-p)
             (pdf-util-pdf-window-p other-win))
        (with-current-buffer (window-buffer other-win)
          (with-selected-window other-win
            (if (>= ln 1)
                (pdf-view-next-line-or-next-page ln)
              (pdf-view-previous-line-or-previous-page (- ln))))
          (set-window-point other-win (point)))
      (scroll-other-window ln))))

(defalias 'modi/scroll-other-window-up 'modi/scroll-other-window)

(defun modi/scroll-other-window-down (ln)
  "Scroll other window down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (modi/scroll-other-window (- ln)))

;; Below bindings are made in global map and not in my minor mode as I want
;; to allow other modes to override these.
(bind-keys
 ("<C-M-up>"    . modi/scroll-down)
 ("<C-M-down>"  . modi/scroll-up)
 ("<C-M-left>"  . modi/scroll-other-window-down)
 ("<C-M-right>" . modi/scroll-other-window-up))

;;;; Mouse Scrolling
(bind-keys
 ("<mouse-4>" . modi/scroll-down)
 ("<mouse-5>" . modi/scroll-up))

(bind-keys
 ;; Make Alt+mousewheel scroll the other buffer
 ("<M-mouse-4>" . modi/scroll-other-window-down) ; M + wheel up
 ("<M-mouse-5>" . modi/scroll-other-window-up)) ; M + wheel down

;; Allow scrolling of all buffers using mouse-wheel in `scroll-all-mode'.
;; By default, `scroll-all-mode' works only with C-v/M-v.
(defun modi/advice-mwhell-scroll-all (orig-fun &rest args)
  "Execute ORIG-FUN in all the windows."
  (let (ret)
    (if scroll-all-mode
        (save-selected-window (walk-windows (lambda (win)
                                              (select-window win)
                                              (condition-case nil
                                                  (setq ret (apply orig-fun args))
                                                (error nil)))))
      (setq ret (apply orig-fun args)))
    ret))
(advice-add 'scroll-up   :around #'modi/advice-mwhell-scroll-all)
(advice-add 'scroll-down :around #'modi/advice-mwhell-scroll-all)
