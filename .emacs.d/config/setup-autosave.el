;;; Auto save files directly, instead of creating those #filename#
;; Auto save often

;; Save every 20 characters typed (this is the minimum)
;; (setq auto-save-interval 20)

;; Save buffer when focus is lost
(use-package focus-autosave-mode
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

;;; Rings and registers
(validate-setq
 kill-ring-max 200                           ; More killed items
 kill-do-not-save-duplicates t               ; No duplicates in kill ring
 ;; Save the contents of the clipboard to kill ring before killing
 save-interprogram-paste-before-kill t)
