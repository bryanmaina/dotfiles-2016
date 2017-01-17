;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (validate-setq vc-follow-symlinks t))

(use-package what-the-commit            ; Insert random commit messages
  :ensure t
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  ;; Shut up, Magit
  (validate-setq
   magit-completing-read-function #'ivy-completing-read
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   ;; Use separate buffers for one-file logs so that we don't need to reset
   ;; the filter everytime for full log view
   magit-log-buffer-file-locked t
   ;; This is creepy, Magit
   magit-revision-show-gravatars nil
   ;; Show status buffer in fullscreen
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   )

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known
  ;; projects.  Initialize the `magit-repository-directories'
  ;; immediately after Projectile was loaded, and update it every time
  ;; we switched projects, because the new project might have been
  ;; unknown before
  (defun lunaryorn-magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because
      ;; Magit adds trailing slashes again, which breaks the
      ;; presentation in the Magit prompt.
      (validate-setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (lunaryorn-magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'lunaryorn-magit-set-repo-dirs-from-projectile))

(use-package projectile-git-autofetch   ; Auto-fetch Github repos
  :ensure t
  :after magit
  :config
  (projectile-git-autofetch-mode)
  ;; Fetch all open projects every ten minutes
  (validate-setq projectile-git-autofetch-projects 'open
                 projectile-git-autofetch-interval 600)
  :diminish (projectile-git-autofetch-mode . "↓"))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c g t" . git-timemachine)));;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (validate-setq vc-follow-symlinks t))

(use-package what-the-commit            ; Insert random commit messages
  :ensure t
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  ;; Shut up, Magit
  (validate-setq
   magit-completing-read-function #'ivy-completing-read
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   ;; Use separate buffers for one-file logs so that we don't need to reset
   ;; the filter everytime for full log view
   magit-log-buffer-file-locked t
   ;; This is creepy, Magit
   magit-revision-show-gravatars nil
   ;; Show status buffer in fullscreen
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   )

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known
  ;; projects.  Initialize the `magit-repository-directories'
  ;; immediately after Projectile was loaded, and update it every time
  ;; we switched projects, because the new project might have been
  ;; unknown before
  (defun lunaryorn-magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because
      ;; Magit adds trailing slashes again, which breaks the
      ;; presentation in the Magit prompt.
      (validate-setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (lunaryorn-magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'lunaryorn-magit-set-repo-dirs-from-projectile))

(use-package projectile-git-autofetch   ; Auto-fetch Github repos
  :ensure t
  :after magit
  :config
  (projectile-git-autofetch-mode)
  ;; Fetch all open projects every ten minutes
  (validate-setq projectile-git-autofetch-projects 'open
                 projectile-git-autofetch-interval 600)
  :diminish (projectile-git-autofetch-mode . "↓"))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c g t" . git-timemachine)))
