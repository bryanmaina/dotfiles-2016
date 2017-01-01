;;; package --- Summary

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;; Commentary:

;; Track servers for emacs daemon
(require 'server)
(unless (server-running-p)
  (server-start))


;; Disable menu bar
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)


;; Use package macro
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)


;; color theme and menubar
(use-package color-theme
  :ensure t)
(require 'color-theme)
(color-theme-initialize)
;; (color-theme-sitaramv-solaris)
(color-theme-snowish)


;; Restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))


;; auto completion
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2
        company-require-match 'never
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-tooltip-limit 20                       ; bigger popup window
        company-tooltip-align-annotations 't           ; align annotations to the right tooltip border
       ;; company-idle-delay .4                          ; decrease delay before autocompletion popup shows
        company-begin-commands '(self-insert-command)  ; start autocompletion only after typing
	company-auto-complete nil
	company-frontends '(company-echo-metadata-frontend
			    company-pseudo-tooltip-unless-just-one-frontend-with-delay
			    company-preview-frontend))
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
       (define-key company-active-map (kbd "C-w") 'backward-kill-word)
       (define-key company-active-map (kbd "M-.") 'company-show-location)
       (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
       (define-key company-active-map (kbd "SPC") 'company-abort)))
  :config
  ;; Java autocompletion
  (use-package company-emacs-eclim
    :ensure t
    :config
    (company-emacs-eclim-setup))
  ;; Javascript autocompletion
  (use-package tern :ensure t :defer t
    :init (add-hook 'javascript-hook 'tern-mode)
    (add-to-list 'company-backends 'company-tern)))

(global-company-mode t)

;; Setup  Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(use-package js3-mode
  :ensure t
  :mode ("\\.js$" . js3-mode))


;; snipets for Angularjs
(use-package yasnippet :ensure t :defer 30
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (setq yas-fallback-behavior 'indent-line
        yas-snippet-dirs '("~/.emacs.d/snippets/angular/")))


;; typescript mode setup
(use-package typescript-mode
  :ensure t)
(require 'typescript-mode)


;; angular2.js apps with ng2-mode
(use-package ng2-mode
  :ensure t)
(require 'ng2-mode)


;; managing buffers
(use-package helm
  :ensure t
  :bind (("C-x b" . helm-buffers-list)
	 ("C-x r b" . helm-bookmarks)
	 ("M-y" . helm-show-kill-ring)))


;; Undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))


;; jump to a character or a line
(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'at-full)
  (setq avy-background t)
  :bind* (("M-s" . avy-goto-char-timer)
          ("M-m" . avy-goto-line)))


;; to search in files
(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ;; ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f6>" . ivy-resume)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))




;; Mostly opening files and
;; killing files
(use-package recentf
  :defer 1
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 200)))

;;; Reopen Killed File
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the `killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (if killed-file-list
      (find-file (pop killed-file-list))
    (message "No recently killed file found to reopen.")))

;;; Current File Buffer Actions
;; Delete current buffer file
(defun modi/delete-current-buffer-file ()
  "Deletes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-exists-p filename)
               (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (message "File `%s' successfully deleted." filename))
    (kill-buffer (current-buffer))))

;; Rename current buffer file
;; http://www.whattheemacsd.com/
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer `%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named `%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File `%s' successfully renamed to `%s'."
                   name (file-name-nondirectory new-name)))))))

;; Display the file path of the file in current buffer and also copy it to
;; the kill-ring
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun modi/copy-buffer-file-name (arg)
  "Show the full path to the current file in the minibuffer and also copy it.
If the full file path has a sub-string \"_xyz\" where xyz is the user name,
replace that with \"_${USER}\".
    C-u COMMAND -> Copy only the file name (not the full path).
C-u C-u COMMAND -> Copy the full path without env var replacement."
  (interactive "p")
  (let* ((file-name-full (buffer-file-name))
         (file-name (when file-name-full
                      (cl-case arg
                        (4 (file-name-nondirectory file-name-full)) ; C-u
                        (16 file-name-full) ; C-u C-u
                        (t ; If $USER==xyz, replace _xyz with _${USER} in file name
                         (replace-regexp-in-string ; no prefix
                          (concat "_" (getenv "USER")) "_${USER}" file-name-full))))))
    (if file-name
        (progn
          (kill-new file-name)
          (message "Copied file name `%s'" file-name))
      (error "Buffer not visiting a file"))))

;;; Revert buffer
(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; (message "buf:%s  filename:%s  modified:%s  filereadable:%s"
      ;;          buf filename
      ;;          (buffer-modified-p buf) (file-readable-p (format "%s" filename)))

      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun modi/revert-noconfirm-help-buffers (&rest args)
  "Don't confirm when reverting *Help* buffers."
  (list (car args) :noconfirm))
(advice-add 'help-mode-revert-buffer :filter-args #'modi/revert-noconfirm-help-buffers)



;;; Minibuffer and Recursive Edit
;; Quit the minibuffer automatically when focus moves away from it (which could
;; have happened by actions like clicking some other buffer using the mouse or
;; by hitting `C-x o'). This is to avoid the irritating occasions where repeated
;; `C-g' pressing doesn't kill the minibuffer prompt as emacs has entered a
;; recursive edit session.
;; http://stackoverflow.com/a/3024055/1219634
;; The right way to exit a recursive edit session is by hitting `C-]', which is
;; bound to `abort-recursive-edit' by default.
(defun abort-recursive-edit-in-minibuffer ()
  "Disable recursive edit in minibuffer if `disable-recursive-edit-in-minibuffer'
is set to a non-nil value."
  (when (and (bound-and-true-p disable-recursive-edit-in-minibuffer)
             (active-minibuffer-window)
             (>= (recursion-depth) 1))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'abort-recursive-edit-in-minibuffer)

;; http://oremacs.com/2016/06/06/counsel-set-variable/
(when (not (bound-and-true-p disable-recursive-edit-in-minibuffer))
  ;; Allow to read from minibuffer while in minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Show the minibuffer depth (when larger than 1)
  (minibuffer-depth-indicate-mode 1))


;;; Toggle between buffers
;; http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers"
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

; xterm mouse support
(require 'mouse)
(xterm-mouse-mode t)

;;;; Mouse Scrolling
(bind-keys
 ("<mouse-4>" . modi/scroll-down)
 ("<mouse-5>" . modi/scroll-up))

(bind-keys
 ;; Make Alt+mousewheel scroll the other buffer
 ("<M-mouse-4>" . modi/scroll-other-window-down) ; M + wheel up
 ("<M-mouse-5>" . modi/scroll-other-window-up)) ; M + wheel down

;; hydra settings and
;; windows managing
(use-package hydra
  :ensure t)

(use-package winner
  :ensure t
  :config
  (progn
    (winner-mode 1)))

(use-package windmove
  :ensure t)

(use-package transpose-frame
  :ensure t)

(use-package ace-window
  :ensure t)

(global-set-key
 (kbd "C-z")
 (defhydra hydra-vi ()
   "vi"
   ("l" forward-char)
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("w" forward-word)
   ("b" backward-word)
   ("W" forward-symbol)
   ("B" (lambda ()
	  (interactive)
	  (forward-symbol -1)))
   ("g" beginning-of-buffer "top")
   ("G" end-of-buffer "bottom")
   ("SPC" set-mark-command)
   ))


(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


(defun hydra-move-splitter-left (delta)
  "Move window splitter left."
  (interactive "p")
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'right)
	(shrink-window-horizontally delta)
      (enlarge-window-horizontally delta))))

(defun hydra-move-splitter-right (delta)
  "Move window splitter right."
  (interactive "p")
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'right)
	(enlarge-window-horizontally delta)
      (shrink-window-horizontally delta))))

(defun hydra-move-splitter-up (delta)
  "Move window splitter up."
  (interactive "p")
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'up)
	(enlarge-window delta)
      (shrink-window delta))))


(defun hydra-move-splitter-down (delta)
  "Move window splitter down."
  (interactive "p")
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'up)
	(shrink-window delta)
      (enlarge-window delta))))


(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:color red)
   "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_h_ ←             _v_ertical      _b_uffer       _a_ X←
_j_ ↓             _x_ horizontal  _f_ind files   _z_ X↓
_k_ ↑             _w_ undo        _q_ ace 1        _e_ X↑
_l_ →             _W_ reset       _s_wap         _r_ X→
_F_ollow          _D_lt Other     _S_ave         max_i_mize
_SPC_ cancel      _o_nly this     _d_elete       _=_ balance
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("a" hydra-move-splitter-left)
   ("z" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-moéve-splitter-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("q" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
    )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
    )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
    )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
    )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("w" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
    )
   ("W" winner-redo)
   ("=" balance-window)
   ("SPC" nil)))

;; (global-set-key (kbd "C-M-o") #'hydra-window/body)


(global-set-key
 (kbd "C-M-s")
 (defhydra hydra-splitter ()
   "splitter"
   ("a" hydra-move-splitter-left "S←")
   ("w" hydra-move-splitter-down "S↓")
   ("e" hydra-move-splitter-up "S↑")
   ("r" hydra-move-splitter-right "S→")
   ))
     

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
(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-emacs-eclim restart-emacs use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
