(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(define-key global-map (kbd "C-c a") 'org-agenda)
;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; every time a task is DONE put it in the logs
(setq org-log-done t)
;; all log into the drawers so that I can hide then
(setq org-log-into-drawer t)
;; special drawer for the clocks
(setq org-clock-into-drawer "CLOCK")
(setq org-agenda-include-diary nil)

;; If you would like to see entities displayed as UTF-8 characters
(setq org-pretty-entities t)

;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))

;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))


(setq org-timeline-show-empty-dates t)
(setq org-insert-mode-line-in-empty-file t)
(setq org-enforce-todo-dependencies t)
(setq before-save-hook 'org-table-recalculate-buffer-tables)


(setq org-todo-keywords
      '((sequence
	 "CANCELLED(c)"
	 "FEEDBACK(f)"
	 "BLOCKED(B)"
	 "WAIT(w)"
	 "IN-PROGRESS(p)"
	 "|"
	 "DONE(d)")))



(setq org-todo-keyword-faces
      '(
        ("BLOCKED" . (:foreground "braun" :weight bold))
        ("DONE" . (:background "lime green" :foreground "yellow" :weight bold  :box (:line-width 2 :style released-button)))
        ("IN-PROGRESS" . (:background "blue" :foreground "white" :weight bold :box (:line-width 2 :style released-button)))
        ("CANCELLED" . (:foreground "red" :weight bold))))



(setq org-agenda-custom-commands
      '(("@" "SOCIAL"((tags "SOCIAL")))
	("P" "Projects"
	 ((agenda)
          (tags-todo "IN-PROGRESS")
          (tags-todo "Bryan")
          (tags-todo "FEEDBACK")
          (tags-todo "VERIFY")))
	("D" "Daily Action List"
	 ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))))))

(defun gtd ()
  (interactive)
  (find-file "~/gtd/resume-tasks.org"))

(defun lopsr ()
  (interactive)
  (find-file "~/gtd/lopsr/lopsr.org"))


;; C-c c is for capture, it’s good enough for me
(define-key global-map (kbd "C-c c") 'org-capture)
;; force UTF-8
(setq org-export-coding-system 'utf-8)


(setq org-capture-templates 
      (quote
       (("a" "Assigned to me"
	 entry (file+headline "~/gtd/personal/personal.org" "Calendar")
	 "* Bryan %^{Description} %^g\n :LOGBOOK:\n - Added: %U %i\n :END:"
	 : empty-lines  1))))
