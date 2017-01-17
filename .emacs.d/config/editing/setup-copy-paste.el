;; enable clipboard in emacs
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)

;; Save text copied from an external program to the kill ring before killing
;; new text from within emacs.
(setq save-interprogram-paste-before-kill t)

;;; Delete Selection
;; Typing anything after highlighting text overwrites that text
;; http://emacsredux.com/blog/2013/04/12/delete-selection-on-insert
(delete-selection-mode 1)

;; paste selected texte from others apps with shift + middle-click
(global-set-key [mouse-2] 'mouse-yank-primary)
