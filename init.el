(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(set-face-attribute 'default nil :font "Jetbrainsmono-12")

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

(use-package org
  :straight (:type built-in))

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
