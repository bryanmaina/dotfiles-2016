(use-package hydra
  :ensure t
  :config
  (setq hydra-is-helpful nil))

(use-package windmove
  :ensure t)

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package ace-window
  :ensure t)

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
 (defhydra hydra-window (:color amaranth :hint nil)
   "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_h_ ←             _v_ertical      _b_uffer       _a_ X←
_j_ ↓             _x_ horizontal  _f_ind files   _z_ X↓
_k_ ↑             _w_ undo        _q_ ace 1      _e_ X↑
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
   ("r" hydra-move-splitter-right)
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
