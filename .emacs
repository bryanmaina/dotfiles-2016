;;; Package --- summary
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/"))


;; Consider header files to be C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Use package macro
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Diminish minor modes from the mode line
(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)

(defun sk/diminish-org-indent ()
  (interactive)
  (diminish 'org-indent-mode ""))
(add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)


(defun sk/diminish-auto-revert ()
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)


(defun sk/diminish-eldoc ()
  (interactive)
  (diminish 'eldoc-mode ""))
(add-hook 'eldoc-mode-hook 'sk/diminish-eldoc)


(defun sk/diminish-subword ()
  (interactive)
  (diminish 'subword-mode ""))
(add-hook 'subword-mode-hook 'sk/diminish-subword)


;; Manage the built-in flyspell mode
(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :bind* (("M-m ] s" . flyspell-goto-next-error)))


;; Which key
(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings"))

;; Hydras
(use-package hydra
  :ensure t)

;; Disabling some GUI elements
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))

;; initial window
(setq initial-frame-alist
      '((width . 102)   ; characters in a line
        (height . 54))) ; number of lines

;; sebsequent frame
(setq default-frame-alist
      '((width . 100)   ; characters in a line
        (height . 52))) ; number of lines

;; Recenter screen
(setq recenter-positions '(top middle bottom))

;; PDF files
(setq doc-view-continuous t)

(setq ns-use-native-fullscreen nil)

;; Bar cursor
;; (setq-default cursor-type '(bar . 1))
;; Don't blink the cursor
;; (blink-cursor-mode -1)

;; Fonts
(cond ((eq system-type 'gnu/linux)
       ;;       (set-frame-font "DejaVu Sans Mono"))
       (set-frame-font "Iosevka"))
      ((eq system-type 'darwin)
       (set-frame-font "Monaco"))
      ((eq system-type 'windows-nt)
       (set-frame-font "Lucida Sans Typewriter")))
;; (cond ((eq system-type 'gnu/linux)
;; 	   (set-frame-font "Iosevka")))


;;; Some convenience font functions
(defun sk/courier-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Courier")
    (set-frame-width (selected-frame) 97))
(defun sk/georgia-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Georgia" :height 160))
(defun sk/hack-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Hack"))
(defun sk/monaco-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaco"))
(defun sk/consolas-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Consolas"))
(defun sk/deja-vu-font ()
  (interactive)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Font types
(defun sk/tiny-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 150))
(defun sk/miniscule-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 97))
(defun sk/small-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 190)
  (set-frame-width (selected-frame) 89))
(defun sk/medium-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 215)
  (set-frame-width (selected-frame) 89))
(defun sk/large-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 350)
  (set-frame-width (selected-frame) 68))


;; async, s, dash, and cl-lib are libraries for asynchronous processing, string
;; manipulation, list manipulation and backward compatibility respectively.
(use-package async
  :ensure t
  :commands (async-start))

(use-package cl-lib
  :ensure t)

(use-package dash
  :ensure t)

(use-package s
  :ensure t)

;; Restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

;; Goto the last change
(use-package goto-chg
  :ensure t
  :bind* (("M-m g ;" . goto-last-change)
          ("M-m g ," . goto-last-change-reverse)))

;; Avy
(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  (setq avy-style 'pre)
  :bind* (("M-m f" . avy-goto-char-timer)
          ("M-m F" . avy-goto-line)))

;; Code documentation
(use-package dash-at-point
  :ensure t
  :bind (("C-c I" . dash-at-point))
  :bind* (("M-m SPC i" . dash-at-point-with-docset)
          ("M-m SPC I" . dash-at-point)))

;; Commenting
(use-package comment-dwim-2
  :ensure t
  :bind* (("M-m g c" . comment-dwim-2)))

;; Interactive edit
(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :bind* (("M-m *" . iedit-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind* (("M-m ." . mc/edit-lines)
          ("M-m >" . mc/mark-next-like-this)
          ("M-m ," . mc/skip-to-next-like-this)
          ("M-m <" . mc/mark-previous-like-this)))

;; Region bindings mode
(use-package region-bindings-mode
  :ensure t
  :demand t
  :bind (:map region-bindings-mode-map
              ("<" . mc/mark-previous-like-this)
              ("," . mc/skip-to-next-like-this)
              (">" . mc/mark-next-like-this)
              ("." . mc/edit-lines))
  :diminish (region-bindings-mode . "ρ")
  :config
  (progn
    (add-hook 'after-init-hook 'region-bindings-mode-enable)))

;; Rename the current buffer and the file associated with it
(defun sk/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(bind-keys*
 ("M-m g R" . sk/rename-current-buffer-file))

;; Delete the current buffer and the file associated with it
(defun sk/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(bind-keys*
  ("M-m g K" . sk/delete-current-buffer-file))


;; Duplicate line or region
(defun sk/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun sk/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (sk/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun sk/duplicate-line-or-region (&optional num)
  "Duplicate the current line or region if active"
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (sk/duplicate-region num beg end)))
  (sk/duplicate-current-line num))

(bind-keys*
 ("M-m g d" . sk/duplicate-line-or-region))

;; Select the current line
(defun sk/select-inside-line ()
  "Select the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (set-mark (line-end-position))
  (exchange-point-and-mark))

(defun sk/select-around-line ()
  "Select line including the newline character"
  (interactive)
  (sk/select-inside-line)
  (next-line 1)
  (sk/smarter-move-beginning-of-line 1))
(bind-keys*
  ("M-m i l" . sk/select-inside-line)
  ("M-m a l" . sk/select-around-line))
(which-key-add-key-based-replacements
  "i l" "select inside line"
  "a l" "select around line")

;; Volatile highlights
(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; Highlight thing at point
(use-package highlight-thing
  :ensure t
  :diminish highlight-thing-mode
  :bind* (("M-m g *" . highlight-thing-mode)))
(which-key-add-key-based-replacements
 "M-m g *" "highlight symbol")

;; Highlight indentation
(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;; Emacs Lisp
(use-package emacs-lisp-mode
  :mode ("\\.el$" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c I" . describe-function)
              ("C-c S" . find-function-at-point)))

;; Auto compile byte recompiles files if they are byte-compiled already.
(use-package auto-compile
  :ensure t)

;; Compile commands for c++
(defun sk/compile-cpp-omp-math ()
  "Compiles the file with OpenMP and math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -lgsl -lcblas -llapack -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-omp-simple ()
  "Compiles the file with OpenMP"
  (interactive)
  (compile
   (concat "g++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-mpi-math ()
  "Compiles the file with MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/mpic++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-mpi-simple ()
  "Compiles the file with MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-hybrid-math ()
  "Compiles the file with OpenMP, MPI and math libraries"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-hybrid-simple ()
  "Compiles the file with OpenMP and MPI"
  (interactive)
  (compile
   (concat "/usr/local/openmpi/bin/c++ -Wall -fopenmp -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-math ()
  "Compiles the file with math libraries"
  (interactive)
  (compile
   (concat "g++ -Wall -lgsl -lcblas -llapack -larmadillo -O2 -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))
(defun sk/compile-cpp-simple ()
  "Compiles the file"
  (interactive)
  (compile
   (concat "g++ -Wall -g -std=c++11 " (buffer-file-name) " -o " (file-name-sans-extension buffer-file-name) ".out")))

;; Rtags is an industrial grade indexer based on clang and its integration with Emacs is quite amazing.
(use-package rtags
  :ensure t
  :defer 2
  :bind (:map c++-mode-map
              ("C-c I" . rtags-print-symbol-info)
              ("C-c S" . rtags-find-symbol-at-point))
  :init
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t))

;; This is a package that sets up all the necessary configuration to work with C/C++.
(use-package cmake-ide
  :ensure t
  :defer 2
  :config
  (cmake-ide-setup))

;; Python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"))

;; Anaconda mode
(use-package anaconda-mode
  :ensure t
  :defer 2
  :diminish anaconda-mode
  :diminish anaconda-eldoc-mode
  :bind (:map python-mode-map
              ("C-c I" . anaconda-mode-show-doc)
              ("C-c S" . anaconda-mode-find-definitions))
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)))

;; python Virtual environment support
(use-package pyenv-mode
  :ensure t
  :commands (pyenv-mode
             pyenv-mode-set
             pyenv-mode-unset))

;; Python formatting
(use-package py-yapf
  :ensure t
  :commands (py-yapf-buffer
             py-yapf-enable-on-save))

;; Python formatting
(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode
  :commands (sphinx-doc
             sphinx-doc-mode))

;; Help writing tests for Python
(use-package pytest
  :ensure t
  :commands (pytest-all
             pytest-directory
             pytest-failed
             pytest-module
             pytest-one
             pytest-pdb-all
             pytest-pdb-directory
             pytest-pdb-module
             pytest-pdb-one))

;; Statistics with R and julia
(use-package ess
  :ensure t
  :mode (("\\.r$" . R-mode)
         ("\\.R$" . R-mode)
         ("\\.jl$" . julia-mode))
  :commands (R-mode
             julia-mode
             sk/julia-shell-here
             ess-eval-function
             ess-eval-line
             ess-eval-buffer
             ess-switch-to-ESS)
  :config
  (require 'ess-site))

;; Vertical split julia REPL
(defun sk/julia-shell-here ()
  "opens up a new julia REPL in the directory associated with the current buffer's file."
  (interactive)
  (require 'ess-site)
  (split-window-right)
  (julia)
  (other-window 1))

;; This is a fully featured, supposedly awesome, package to edit HTML in Emacs.
(use-package web-mode
  :ensure t
  ;;:mode ("\\.html$" . web-mode)
  )

;; JavaScript syntax highlighting
(use-package js3-mode
  :ensure t
  :mode ("\\.js$" . js3-mode))

;; Coffeescript syntax highlighting
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee$")

;; SCSS syntax highlighting
(use-package scss-mode
  :ensure t
  :mode "\\.scss$")

;; JSON mode
(use-package json-mode
  :ensure t
  :mode "\\.json$")

;; Nginx syntax highlighting
(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; Write fast HTML with emmet
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point))
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation

;; JavaScript navigation
(use-package tern
  :ensure t
  :diminish tern-mode
  :defer 2
  :config
  (progn
    (add-hook 'js-mode-hook '(lambda () (tern-mode t)))))

;; Skewer JS REPL
(use-package skewer-mode
  :ensure t
  :diminish skewer-mode
  :commands (skewer-mode
             skewer-html-mode
             skewer-css-mode
             run-skewer
             skewer-repl
             list-skewer-clients
             skewer-eval-defun
             skewer-eval-last-expression
             skewer-eval-print-last-expression
             skewer-load-buffer
             skewer-bower-load
             skewer-bower-refresh
             skewer-run-phantomjs
             skewer-phantomjs-kill))

;; Node JS REPL
(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl
             nodejs-repl-send-buffer
             nodejs-repl-switch-to-repl
             nodejs-repl-send-region
             nodejs-repl-send-last-sexp
             nodejs-repl-execute
             nodejs-repl-load-file))

;; Managing node versions with nvm.el.
(use-package nvm
  :ensure t
  :commands (nvm-use
             nvm-use-for))

;; Snatch JSON
(use-package json-snatcher
  :ensure t
  :commands (jsons-print-path))

;; Beautify
(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

;; Error checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer 2
  :bind* (("M-m ] l"   . flycheck-next-error)
          ("M-m [ l"   . flycheck-previous-error)
          ("M-m SPC l" . flycheck-list-errors))
  :config
  (global-flycheck-mode))
(which-key-add-key-based-replacements
  "] l"   "next error"
  "[ l"   "previous error"
  "SPC l" "list errors")

;; Auto completion
(use-package company
  :ensure t
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-ispell
             company-c-headers
             company-jedi
             company-tern
             company-web-html
             company-auctex)
  :init
  (setq company-minimum-prefix-length 2
        company-require-match 0
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-tooltip-limit 20                      ; bigger popup window
        company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
        company-idle-delay .4                         ; decrease delay before autocompletion popup shows
        company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-files
                                      company-capf)))
  :bind (("M-t"   . company-complete)
         ("C-c f" . company-files)
         ("C-c a" . company-dabbrev)
         ("C-c d" . company-ispell)
         :map company-active-map
              ("C-n"    . company-select-next)
              ("C-p"    . company-select-previous)
              ([return] . company-complete-selection)
              ("C-w"    . backward-kill-word)
              ("C-c"    . company-abort)
              ("C-c"    . company-search-abort))
  :diminish (company-mode . "ς")
  :config
  (global-company-mode)
  ;; C++ header completion
  (use-package company-c-headers
    :ensure t
    :bind (("C-c c" . company-c-headers))
    :config
    (add-to-list 'company-backends 'company-c-headers))
  ;; Python auto completion
  (use-package company-jedi
    :ensure t
    :bind (("C-c j" . company-jedi))
    :config
    (add-to-list 'company-backends 'company-jedi))
  ;; Tern for JS
  (use-package company-tern
    :ensure t
    :bind (("C-c t" . company-tern))
    :init
    (setq company-tern-property-marker "")
    (setq company-tern-meta-as-single-line t)
    :config
    (add-to-list 'company-backends 'company-tern))
  ;; HTML completion
  (use-package company-web
    :ensure t
    :bind (("C-c w" . company-web-html))
    :config
    (add-to-list 'company-backends 'company-web-html))
  ;; LaTeX autocompletion
  (use-package company-auctex
    :ensure t
    :bind (("C-c l" . company-auctex))
    :config
    (add-to-list 'company-backends 'company-auctex)))

;; Eshell
(use-package eshell
  :commands (eshell)
  :bind* (("M-m SPC s" . sk/eshell-vertical)
          ("M-m SPC S" . sk/eshell-horizontal))
  :init
  (setq eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'this
        eshell-buffer-shorthand t
        eshell-history-size 1024
        eshell-cmpl-ignore-case t
        eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
        eshell-last-dir-ring-size 512)
  :config
  (add-hook 'shell-mode-hook 'goto-address-mode))
;; Vertical split eshell
(defun sk/eshell-vertical ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-right)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))

;; Horizontal split eshell
(defun sk/eshell-horizontal ()
  "opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-below)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (eshell-send-input)))
(bind-keys*
  ("M-m SPC s" . sk/eshell-vertical)
  ("M-m SPC S" . sk/eshell-horizontal))
(which-key-add-key-based-replacements
  "M-m SPC s" "eshell vertical split"
  "M-m SPC S" "eshell horizontal split")

;; Multi term doesn’t provide any additional commands to built-in Emacs term and ansi-term but helps in managing multiple terminal buffers.
(use-package multi-term
  :ensure t
  :commands (multi-term)
  :bind* (("M-m SPC t" . sk/multi-term-vertical)
          ("M-m SPC T". sk/multi-term-horizontal)))

;; Vertical split multi-term
(defun sk/multi-term-vertical ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-term))

;; Horizontal split multi-term
(defun sk/multi-term-horizontal ()
  "opens up a new terminal in the directory associated with the current buffer's file."
  (interactive)
  (split-window-below)
  (other-window 1)
  (multi-term))

(bind-keys*
  ("M-m SPC t" . sk/multi-term-vertical)
  ("M-m SPC T" . sk/multi-term-horizontal))

(which-key-add-key-based-replacements
  "M-m SPC t" "terminal vertical split"
  "M-m SPC T" "terminal horizontal split")

;; This package helps you quickly run little pieces of code.
(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region))

;; Debugging
(use-package realgud
  :ensure t
  :commands (realgud:gdb
             realgud:ipdb
             realgud:pdb))

;; HYDRAfor Js
(defhydra sk/hydra-for-js (:color blue
                           :hint nil)
  "
 ^Node^                                ^Tern^                                  ^Json^
^^^^^^^^^^-----------------------------------------------------------------------------------------
 _r_: region    _s_: start    _l_: load    _d_: definition    _h_: highlight refs    _j_: path
 _b_: buffer    _S_: switch              _n_: def by name   _u_: use-server        _q_: quit
 _x_: sexp      _e_: exec                _t_: type          _D_: doc
"
  ("r" nodejs-repl-send-region)
  ("b" nodejs-repl-send-buffer)
  ("x" nodejs-repl-send-last-sexp)
  ("s" nodejs-repl)
  ("S" nodejs-repl-switch-to-repl)
  ("e" nodejs-repl-execute)
  ("l" nodejs-repl-load-file :color red)
  ("d" tern-find-definition :color red)
  ("n" tern-find-definition-by-name :color red)
  ("t" tern-get-type :color red)
  ("D" tern-get-docs :color red)
  ("u" tern-use-server :color red)
  ("h" tern-highlight-refs)
  ("R" tern-rename-variable)
  ("j" jsons-print-path)
  ("q" nil :color blue))

;;  HYDRA for Web
(defhydra sk/hydra-for-web (:color red
                            :hint nil)
  "
 ^Server^           ^HTML^                ^Skewer^
^^^^^^^^^^------------------------------------------------------------------------------------------------------------------------------
 _w_: httpd start   _i_: html real-time   _j_: js eval     _r_: run            _f_: eval func              _l_: load buffer      _p_: phantomjs
 _W_: httpd stop                        _h_: html eval   _s_: start          _e_: eval last exp          _b_: bower load       _P_: phantomjs kill
                                      _c_: css eval    _S_: list clients   _E_: eval print last exp    _B_: bower refresh    _q_: quit
"
  ("w" httpd-start :color blue)
  ("W" httpd-stop :color blue)
  ("i" impatient-mode :color blue)
  ("j" skewer-mode)
  ("h" skewer-html-mode)
  ("c" skewer-css-mode)
  ("r" run-skewer)
  ("s" skewer-repl :color blue)
  ("S" list-skewer-clients :color blue)
  ("f" skewer-eval-defun :color blue)
  ("e" skewer-eval-last-expression :color blue)
  ("E" skewer-eval-print-last-expression :color blue)
  ("l" skewer-load-buffer :color blue)
  ("b" skewer-bower-load :color blue)
  ("B" skewer-bower-refresh)
  ("p" skewer-run-phantomjs :color blue)
  ("P" skewer-phantomjs-kill)
  ("q" nil :color blue))

;; HYDRA for fromat
(defhydra sk/hydra-for-format (:color red
                               :hint nil)
  "
 ^Beautify^
^^^^^^^^^^--------------------------------------
 _h_: html        _c_: css       _j_: js        _q_: quit
 _H_: html buf    _C_: css buf   _J_: js buf
"
  ("h" web-beautify-html)
  ("H" web-beautify-html-buffer)
  ("c" web-beautify-css)
  ("C" web-beautify-css-buffer)
  ("j" web-beautify-js)
  ("J" web-beautify-js-buffer)
  ("q" nil :color blue))

(bind-keys*
  ("M-m s j" . sk/hydra-for-js/body)
  ("M-m s w" . sk/hydra-for-web/body)
  ("M-m s f" . sk/hydra-for-format/body))

(which-key-add-key-based-replacements
  "M-m s j" "javscript code"
  "M-m s w" "web code"
  "M-m s f" "format code")

;; HYDRA : This is for Quickrun functionality.
(defhydra sk/hydra-quickrun (:color blue
                             :hint nil)
  "
 _s_: quickrun     _a_: with arg    _c_: compile only       _q_: quit
 _r_: run region   _S_: shell       _R_: replace region
"
  ("s" quickrun)
  ("r" quickrun-region)
  ("a" quickrun-with-arg)
  ("S" quickrun-shell)
  ("c" quickrun-compile-only)
  ("R" quickrun-replace-region)
  ("q" nil :color blue))

(bind-keys*
 ("M-m s q" . sk/hydra-quickrun/body))

(which-key-add-key-based-replacements
  "M-m s q" "quickrun code")

;; HYDRA Debugging
(defhydra sk/hydra-debug (;; :pre (load-library "realgud")
                          :color blue
                          :hint nil)
  "
 _g_: c-gdb         _p_: py-pdb        _i_: py-ipdb        _q_: quit
 _G_: realgud-gdb   _P_: realgud-pdb   _I_: realgud-ipdb
"
  ("g" gdb)
  ("G" realgud:gdb)
  ("p" pdb)
  ("P" realgud:pdb)
  ("i" ipdb)
  ("I" realgud:ipdb)
  ("q" nil :color blue))

(bind-keys*
 ("M-m s d" . sk/hydra-debug/body))

(which-key-add-key-based-replacements
  "M-m s d" "debug code")

;; color theme and menu bar
(require 'color-theme)
(color-theme-initialize)
(color-theme-sitaramv-solaris)
(menu-bar-mode -1)


;; (use-package spaceline :ensure t
;;   :config
;;   (use-package spaceline-config
;;     :config
;;     (spaceline-toggle-minor-modes-off)
;;     (spaceline-toggle-buffer-encoding-off)
;;     (spaceline-toggle-buffer-encoding-abbrev-off)
;;     (setq powerline-default-separator 'slant)
;;     (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;     (spaceline-define-segment line-column
;;       "The current line and column numbers."
;;       "l:%l c:%2c")
;;     (spaceline-define-segment time
;;       "The current time."
;;       (format-time-string "%H:%M"))
;;     (spaceline-define-segment date
;;       "The current date."
;;       (format-time-string "%h %d"))
;;     (spaceline-toggle-time-on)
;;     (spaceline-emacs-theme 'date 'time)))


(use-package powerline
  :config
  (use-package airline-themes
    :config
    (load-theme 'airline-light t)
    (setq powerline-height 20)
    (setq powerline-raw " ")
    (setq ns-use-srgb-colorspace nil)
    (setq powerline-default-separator 'alternate))) 


;; Setting for neotree
(use-package neotree
  :ensure t
  :init
  (progn
    ;; Every time when the neotree window is opened, it will try to find current
    ;; file and jump to node.  
    (setq-default neo-smart-open t)
    ;; Do not allow neotree to be the only open window
    (setq-default neo-dont-be-alone t))
  :config
  (progn
    (setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow
    (setq neo-vc-integration '(face char))))

(add-hook 'neo-change-root-hook
          (lambda () (neo-buffer--with-resizable-window
		      (let ((fit-window-to-buffer-horizontally t))
			(fit-window-to-buffer)))))

(global-set-key [f8] 'neotree-toggle)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "color-233"))))
 '(hl-line ((t (:background "color-233"))))
 '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
 '(neo-dir-link-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "white")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("da538070dddb68d64ef6743271a26efd47fbc17b52cc6526d932b9793f92b718" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "b563a87aa29096e0b2e38889f7a5e3babde9982262181b65de9ce8b78e9324d5" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "256a381a0471ad344e1ed33470e4c28b35fb4489a67eb821181e35f080083c36" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237" "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "228c0559991fb3af427a6fa4f3a3ad51f905e20f481c697c6ca978c5683ebf43" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "cf284fac2a56d242ace50b6d2c438fcc6b4090137f1631e32bedf19495124600" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(helm-ag-base-command "/usr/bin/pt -e --nocolor --nogroup")
 '(package-selected-packages
   (quote
    (green-phosphor-theme color-theme-emacs-revert-theme color-theme-eclipse ts-comint ng2-mode tide undo-tree google-this typit ledger-mode helm-make helm-c-yasnippet helm-flyspell helm-flycheck helm-circe helm-aws helm-descbinds helm-projectile helm-swoop helm-bibtex helm-flx helm-fuzzier helm-smex airline-themes spaceline company projectile cider)))
 '(plantuml-jar-path "/opt/plantuml/plantuml.jar")
 '(sml/mode-width
   (if
       (eq
	(powerline-current-separator)
	(quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote sml/global)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active2)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))

;; cider settings
(unless (package-installed-p 'cider)
  (packafe-refresh-contents)
  (package-install 'cider))
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; rainbow-delimiters settings
(unless (package-installed-p 'rainbow-delimiters)
  (package-refresh-contents)
  (package-install 'rainbow-delimiters))
;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; rainbow-blocks settings
(unless (package-installed-p 'rainbow-blocks)
  (package-refresh-contents)
  (package-install 'rainbow-blocks))
;; (add-hook 'clojure-mode-hook 'rainbow-blocks-mode)

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


;; Convenient for generating UML diagrams from text
(use-package plantuml-mode :ensure t
  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list
   'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))


;; helm-ag settings
(unless (package-installed-p 'helm-ag)
  (package-refresh-contents)
  (package-install 'helm-ag))

(global-set-key (kbd "M-s") 'helm-do-ag)

;; clojure mode
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(add-hook 'clojure-mode-hook 'cider-mode) ; Enter cider mode when entering the clojure major mode



;; relative line numbers
(unless (package-installed-p 'nlinum-relative)
  (package-refresh-contents)
  (package-install 'nlinum-relative))
;; (nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...


(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...
(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                   ;; you will be asked before the abbreviations are saved

;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; Set the default comment column to 70
(setq-default comment-column 70)

;; Every time a window is started, make sure it get maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Use the Input font size 12
;; (set-default-font "Input-12")

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Show parenthesis mode
(show-paren-mode 1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p)
      (move-region-up start end n)
    (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p)
      (move-region-down start end n)
    (move-line-down n)))


(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "M-p") 'move-line-up)

(global-set-key (kbd "C-M-P") 'move-line-region-up)
(global-set-key (kbd "C-M-N") 'move-line-region-down)




;; HELM
(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x 8"   . helm-ucs))
  :bind* (("M-m SPC h r" . helm-resume)
          ("M-m SPC r"   . helm-for-files)
          ("M-m SPC x"   . helm-apropos)
          ("M-m SPC C" . helm-colors)
          ("M-m SPC h R" . helm-regexp)
          ("M-m SPC h u" . helm-surfraw)
          ("M-m SPC h t" . helm-top)
          ("M-m SPC h p" . helm-list-emacs-process)
          ("M-m SPC F"   . helm-find)
          ("M-m SPC h k" . helm-calcul-expression)
          ("M-m SPC h i" . helm-info-at-point)
          ("M-m SPC h d" . helm-man-woman)
          ("M-m SPC h h" . helm-documentation)
          ("M-m SPC h e" . helm-run-external-command)
          ("M-m ;"       . helm-all-mark-rings)
          ("M-m SPC h x" . helm-select-xfont)
          ("M-m t"       . helm-semantic-or-imenu))
  :bind (:map helm-map
              ("<return>"   . helm-maybe-exit-minibuffer)
              ("RET"        . helm-maybe-exit-minibuffer)
              ("<tab>"      . helm-select-action)
              ("C-i"        . helm-select-action)
              ("S-<return>" . helm-maybe-exit-minibuffer)
              ("S-RET"      . helm-maybe-exit-minibuffer)
              ("C-S-m"      . helm-maybe-exit-minibuffer))
  :bind (:map helm-find-files-map
              ("<return>"    . helm-execute-persistent-action)
              ("RET"         . helm-execute-persistent-action)
              ("<backspace>" . dwim-helm-find-files-up-one-level-maybe)
              ("DEL"         . dwim-helm-find-files-up-one-level-maybe)
              ("<tab>"       . helm-select-action)
              ("C-i"         . helm-select-action)
              ("S-<return>"  . helm-maybe-exit-minibuffer)
              ("S-RET"       . helm-maybe-exit-minibuffer)
              ("C-S-m"       . helm-maybe-exit-minibuffer))
  :bind (:map helm-read-file-map
              ("<return>"    . helm-execute-persistent-action)
              ("RET"         . helm-execute-persistent-action)
              ("<backspace>" . dwim-helm-find-files-up-one-level-maybe)
              ("DEL"         . dwim-helm-find-files-up-one-level-maybe)
              ("<tab>"       . helm-select-action)
              ("C-i"         . helm-select-action)
              ("S-<return>"  . helm-maybe-exit-minibuffer)
              ("S-RET"       . helm-maybe-exit-minibuffer)
              ("C-S-m"       . helm-maybe-exit-minibuffer))
  :commands (helm-mode
             helm-M-x
             helm-smex
             helm-find-files
             helm-buffers
             helm-recentf)
  :config
  ;; require basic config
  (require 'helm-config)
  (helm-mode 1)

  ;; use silver searcher when available
  (when (executable-find "ag")
    (setq helm-grep-default-command "ag -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ag -H --no-group --no-color %e %p %f"))

  ;; Fuzzy matching for everything
  (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match nil
        helm-mode-fuzzy-match t)

  ;; set height and stuff
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 20
        helm-autoresize-min-height 20)

  ;; Work with Spotlight on OS X instead of the regular locate
  (setq helm-locate-command "mdfind -name -onlyin ~ %s %s")

  ;; Make sure helm always pops up in bottom
  (setq helm-split-window-in-side-p t)

  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.2)))

  ;; provide input in the header line and hide the mode lines above
  (setq helm-echo-input-in-header-line t)

  (defvar bottom-buffers nil
    "List of bottom buffers before helm session.
      Its element is a pair of `buffer-name' and `mode-line-format'.")

  (defun bottom-buffers-init ()
    (setq-local mode-line-format (default-value 'mode-line-format))
    (setq bottom-buffers
          (cl-loop for w in (window-list)
                   when (window-at-side-p w 'bottom)
                   collect (with-current-buffer (window-buffer w)
                             (cons (buffer-name) mode-line-format)))))

  (defun bottom-buffers-hide-mode-line ()
    (setq-default cursor-in-non-selected-windows nil)
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format nil)))
          bottom-buffers))

  (defun bottom-buffers-show-mode-line ()
    (setq-default cursor-in-non-selected-windows t)
    (when bottom-buffers
      (mapc (lambda (elt)
              (with-current-buffer (car elt)
                (setq-local mode-line-format (cdr elt))))
            bottom-buffers)
      (setq bottom-buffers nil)))

  (defun helm-keyboard-quit-advice (orig-func &rest args)
    (bottom-buffers-show-mode-line)
    (apply orig-func args))

  (add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
  (add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
  (add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
  (add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
  (advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

  ;; remove header lines if only a single source
  (setq helm-display-header-line nil)

  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height 1.0)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))

  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  ;; hide the minibuffer when helm is active
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  ;; Proper find file behavior
  (defun dwim-helm-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-backward-char 1)))

  (defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
    "Adjust how helm-execute-persistent actions behaves, depending on context"
    (if (file-directory-p (helm-get-selection))
        (apply orig-fun args)
      (helm-maybe-exit-minibuffer)))

  (advice-add 'helm-execute-persistent-action :around #'dwim-helm-find-files-navigate-forward)

  ;; better smex integration
  (use-package helm-smex
    :ensure t
    :bind* (("M-x" . helm-smex)
            ("M-X" . helm-smex-major-mode-commands)))

  ;; Make helm fuzzier
  (use-package helm-fuzzier
    :ensure t
    :config
    (helm-fuzzier-mode 1))

  ;; Add support for flx
  (use-package helm-flx
    :ensure t
    :config
    (helm-flx-mode 1))

  ;; Add helm-bibtex
  (use-package helm-bibtex
    :ensure t
    :bind* (("M-m SPC b" . helm-bibtex))
    :init
    (setq bibtex-completion-bibliography '("~/Dropbox/org/references/multiphysics.bib" "~/Dropbox/org/references/chanceconstraints.bib" "~/Dropbox/org/references/tensors.bib"))
    (setq bibtex-completion-library-path "~/Dropbox/org/references/pdfs")
    (setq bibtex-completion-notes-path "~/Dropbox/org/references/articles.org"))

  ;; to search in projects - the silver searcher
  (use-package helm-ag
    :ensure t
    :bind* (("M-m g s" . helm-do-ag-project-root)
            ("M-m g e" . helm-do-ag)))

  ;; to search in files
  (use-package helm-swoop
    :ensure t
    :bind (("C-s" . helm-swoop-without-pre-input))
    :bind* (("M-m #"   . helm-swoop)
            ("M-m g /" . helm-multi-swoop)
            ("M-m o /" . helm-multi-swoop-org)
            ("M-m g E" . helm-multi-swoop-all))
    :init
    (setq helm-swoop-split-with-multiple-windows nil
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-split-window-function 'helm-default-display-buffer))

  ;; to help with projectile
  (use-package helm-projectile
    :ensure t
    :bind* (("M-m SPC d" . helm-projectile))
    :init
    (setq projectile-completion-system 'helm))

  ;; to describe bindings
  (use-package helm-descbinds
    :ensure t
    :bind* (("M-m SPC ?" . helm-descbinds)))

  ;; Control AWS via helm
  (use-package helm-aws
    :ensure t
    :bind* (("M-m SPC h w" . helm-aws)))

  ;; Control circe with helm
  (use-package helm-circe
    :ensure t
    :bind* (("M-m SPC h j" . helm-circe)
            ("M-m SPC h J" . helm-circe-new-activity)))

  ;; List errors with helm
  (use-package helm-flycheck
    :ensure t
    :bind* (("M-m SPC l" . helm-flycheck)))

  ;; Flyspell errors with helm
  (use-package helm-flyspell
    :ensure t
    :bind* (("M-m SPC h s" . sk/helm-correct-word))
    :config
    (defun sk/helm-correct-word ()
      (interactive)
      (save-excursion
        (sk/flyspell-goto-previous-error 1)
        (helm-flyspell-correct))))

  ;; Select snippets with helm
  (use-package helm-c-yasnippet
    :ensure t
    :bind (("C-o" . helm-yas-complete))
    :bind* (("C-,"        . helm-yas-create-snippet-on-region)
            ("C-<escape>" . helm-yas-visit-snippet-file)))

  ;; Helm integration with make
  (use-package helm-make
    :ensure t
    :init
    (setq helm-make-build-directory "build")
    :bind* (("M-m SPC m" . helm-make-projectile)
            ("M-m SPC M" . helm-make))))

(which-key-add-key-based-replacements
  "t"       "tags/func in buffer"
  "#"       "swoop at point"
  ";"       "previous edit points"
  "g E"     "extract word from buffers"
  "g s"     "search project"
  "g /"     "multi file search"
  "o /"     "org swoop"
  "g e"     "extract word from dir"
  "SPC r"   "find any file"
  "SPC C"   "color picker"
  "g u"     "simulate C-c C-e"
  "SPC b"   "bibliography"
  "SPC x"   "helm apropos"
  "SPC J"   "helm major mode cmds"
  "SPC F"   "find command"
  "SPC h"   "helm prefix"
  "SPC h r" "resume last helm "
  "SPC h e" "external command"
  "SPC h w" "AWS instances"
  "SPC h i" "information at point"
  "SPC h R" "build regexp"
  "SPC h u" "surfraw"
  "SPC h t" "system processes"
  "SPC h p" "emacs processes"
  "SPC h k" "calc expression"
  "SPC h d" "manual docs"
  "SPC h h" "helm docs"
  "SPC h x" "select font"
  "SPC h j" "circe chat"
  "SPC h J" "circe new activity"
  "SPC h s" "helm spelling"
  "SPC m" "make in project"
  "SPC M" "make in current dir")


;; Keep track of accounts
(use-package ledger-mode
  :ensure t
  :mode "\\.dat$")

;; Touch typing
(use-package typit
  :ensure t
  :commands (typit))

;; Google stuff
(use-package google-this
  :ensure t
  :commands (google-this-word
             google-this-region
             google-this-symbol
             google-this-clean-error-string
             google-this-line
             google-this-search
             google-this-cpp-reference))

(defun sk/google-this ()
  "Google efficiently"
  (interactive)
  (if (region-active-p)
      (google-this-region 1)
    (google-this-symbol 1)))

(bind-keys*
 ("M-m A" . sk/google-this))

(which-key-add-key-based-replacements
  "A" "ask google")

;; A hydra for googling things!
(defhydra sk/hydra-google (:color blue
                           :hint nil)
  "
 _w_: word   _r_: region    _v_: symbol   _l_: line
 _g_: google _c_: cpp       _s_: string   _q_: quit
 "
  ("w" google-this-word)
  ("r" google-this-region)
  ("v" google-this-symbol)
  ("s" google-this-clean-error-string)
  ("l" google-this-line)
  ("g" google-this-search)
  ("c" google-this-cpp-reference)
  ("q" nil :color blue))

(bind-keys*
 ("M-m g G" . sk/hydra-google/body))

(which-key-add-key-based-replacements "M-m g G" "google now")


;; Undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

(which-key-add-key-based-replacements
  "u" "undo"
  "r" "redo"
  "U" "undo tree")

;; TYPESCRIPT
(use-package typescript-mode
  :ensure t)

;; a Typescript REPL in Emac
;; M-x run-ts to create a comint buffer with the Typescript interpreter
(use-package ts-comint
  :ensure t
  :config
  (progn
    (add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))))

;; Tide for typescript
(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; tide javaScript setting
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; tide TSX setting
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; tide JSX setting
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; Debugging typescript with tide
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

;; angular2.js apps with ng2-mode
(use-package ng2-mode
  :ensure t)

