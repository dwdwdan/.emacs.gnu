(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(set-face-attribute 'default nil :font "Jetbrainsmono-12")
(cond ((eq system-type 'windows-nt)
       (setq sync-dir "C:/Users/Daniel Walters/Dropbox")
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'gnu/linux)
       (setq sync-dir "~/Nextcloud")
       ;; Linux-specific code goes here.
       ))

;; Straight.el Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package general
  :config
    (general-create-definer my/leader
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC"))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init (evil-collection-init))

(use-package helm
  :general
  ("M-x" 'helm-M-x)
  ("C-x C-f" 'helm-find-files)
  (my/leader
    "." '(helm-find-files :which-key "Find Files")
    "f" '(helm-find-files :which-key "Find Files"))
  :config (helm-mode 1))

(use-package magit
  :general
  (my/leader
    "g" '(magit :which-key "Magit")))

(use-package which-key
  :config (which-key-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info t))


(use-package projectile
  :init
  (projectile-mode 1)
  :general
  ("C-c p" 'projectile-command-map)
  (my/leader
    "p" '(:ignore t :which-key "Projectile")
    "p p" '(projectile-switch-project :which-key "Switch Project")
    "p !" '(projectile-run-shell-command-in-root :which-key "Project Shell Command")
    "p f" '(projectile-find-file :which-key "Find File")
    "SPC" '(projectile-find-file :which-key "Projectile Find File"))
  :custom
  (projectile-project-search-path '("~/repos")))

(use-package helm-projectile
  :after projectile)

(defun my/pdf-hook ()
  (display-line-numbers-mode -1))

(use-package pdf-tools
  :hook
  (pdf-view-mode . my/pdf-hook)
  :init
  (pdf-tools-install))

(my/leader
  "w" '(:ignore t :which-key "Window")
  "w s" '(evil-window-split :which-key "Horizontal Split")
  "w v" '(evil-window-vsplit :which-key "Vertical Split")
  "w h" '(evil-window-left :which-key "Move Left")
  "w j" '(evil-window-down :which-key "Move Down")
  "w k" '(evil-window-up :which-key "Move Up")
  "w l" '(evil-window-right :which-key "Move Right")
  "w q" '(evil-quit :which-key "Close Window")

  "b" '(:ignore t :which-key "Buffer")
  "b b" '(switch-to-buffer :which-key "Switch Buffer"))
(setq org-directory (concat sync-dir "/Org")
      org-roam-directory (concat sync-dir "/OrgRoam")
      org-ellipsis " ▼"
      org-superstar-headline-bullets-list '("◉" "○")
      org-agenda-span 7
      org-agenda-start-on-weekday 1
      org-agenda-start-day "+0d"
      org-log-into-drawer t
      org-startup-with-latex-preview t
      org-agenda-files `(,org-directory))
      
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(defun my/org-config ()
        (dolist (face '((org-level-1 . 1.5)
                        (org-level-2 . 1.4)
                        (org-level-3 . 1.3)
                        (org-level-4 . 1.2)
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.05)))
        (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

        (set-face-attribute 'org-document-title nil :height 300)
        (set-face-attribute 'org-block nil :foreground nil :background "#353848" :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-table nil :background "#353848" :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

        (setq org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)")))
        (setq org-refile-targets `((,(concat org-directory "/archive.org") :maxlevel . 2)
                                   (,(concat org-directory "/todo.org") :maxlevel . 1)))
        (setq org-capture-templates `(("t" "Todo" entry (file ,(concat org-directory "/inbox.org")) "* TODO %?\n %U\n %a\n %i" :empty-lines 1))))

(defun my/org-hook ()
  (variable-pitch-mode 1)
  (org-indent-mode)
  (display-line-numbers-mode -1)
  (org-fragtog-mode))


(use-package org
  :straight (:type built-in)
  :hook
  (org-mode . my/org-hook)
  :config
  (my/org-config))

(use-package org-fragtog)
