(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq split-width-threshold most-positive-fixnum)

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(setq show-paren-style 'expression)

(desktop-save-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(icomplete-mode 1) ;Change what happens on C-x b
(global-set-key (kbd "C-x C-b") 'bs-show)


;; Bootsrap straight.el
(setq straight-repository-branch "master")
(setq straight-use-package-by-default t)

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

(straight-use-package 'use-package)

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package web-mode
  :config
  (setq web-mode-enable-auto-indentation nil)
  )

(use-package yaml-mode
  :mode "\\.yml\\'"
  )

;(use-package vs-dark-theme)
;(use-package dracula-theme)

(use-package ivy
  :disabled
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

(use-package helm
  :disabled
  )

(use-package bm
  :bind
  ("<C-left>" . bm-toggle)
  ("<C-up>" . bm-previous)
  ("<C-down>" . bm-next)
  )

(use-package powerline)
(use-package moe-theme
  :config
  (setq moe-theme-highlight-buffer-id t)
;  (moe-theme-set-color 'orange)
  (moe-dark)
  (powerline-moe-theme)
  )

(use-package amx
  :config
  (amx-mode)
  )

(use-package projectile
  :disabled
  :config
  (projectile-mode +1)
  :bind
  ("C-p" . projectile-command-map)
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)
