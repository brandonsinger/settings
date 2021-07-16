(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
;(setq split-width-threshold most-positive-fixnum)

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
;(setq show-paren-style 'expression)

(setq-default tab-width 4)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x <") 'org-insert-structure-template)

(desktop-save-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

(defun echo/org-mode-setup ()
    (org-indent-mode)
    (visual-line-mode 1)
    )
  (use-package org
    :hook (org-mode . echo/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    )

;  (use-package org-bullets
;        :after org
;        :hook (org-mode . org-bullets-mode)
;        :custom
;        (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
;        )

  (defun echo/org-mode-visual-fill ()
        (setq visual-fill-column-width 100
              visual-fill-column-center-text t)
        (visual-fill-column-mode 1))
  (use-package visual-fill-column
        :hook (org-mode . echo/org-mode-visual-fill))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

(defun echo/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/settings/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'echo/org-babel-tangle-config))
)

(use-package org-contrib
      :after org
      :config
      (require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("py" . "src python"))
      )

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line))
  )

(use-package magit
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
    )

(use-package web-mode
  :config
  (setq web-mode-enable-auto-indentation nil)
  )

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(use-package python-mode
  :ensure nil
  :custom
  (python-shell-interperter "python")
  )

;; Todo
(require 'web-mode)
(setq web-mode-enable-auto-indentation nil)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php[s34]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  )

(use-package amx
      :config
      (amx-mode)
      )

(use-package projectile
      :diminish projectile-mode
      :config
      (projectile-mode)
      :custom
      ((projectile-completion-system 'ivy))
      :bind
      ("C-p" . projectile-command-map)
      :init
      (when (file-directory-p "~/projects")
        (setq projectile-project-search-path '("~/projects")))
      )

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode)
  )

(use-package ivy
:diminish
:bind (
       ("C-s" . swiper)
       :map ivy-switch-buffer-map
       ("C-k" . (lambda()
                  (interactive)
                  (ivy-set-action 'ivy-switch-buffer-kill)
                  (ivy-done)
                  ))
       )
:config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-wrap t)
)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after
  (ivy)
    )

(use-package hydra)

(use-package ivy-hydra
  :after
  (ivy hydra)
  )

(use-package counsel
      :bind (
             ("M-x" . counsel-M-x)
             ("C-x b" . counsel-ibuffer)
             ("C-x C-b" . counsel-ibuffer)
             ("C-x C-f" . counsel-find-file)
             :map minibuffer-local-map
             ("C-r" . 'counsel-minibuffer-history)
             )
      )

(use-package bm
  :bind
  ("<C-left>" . bm-toggle)
  ("<C-up>" . bm-previous)
  ("<C-down>" . bm-next)
  )

(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete)
  :config
  (setq switch-window-minibuffer-shortcut ?z)
  (setq switch-window-shortcut-appearance 'asciiart)
  )

(use-package doom-themes
  :config
  ;(load-theme 'doom-gruvbox)
  (load-theme 'doom-dracula)
  )
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

(setq visible-bell t)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook)
              )
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b7e460a67bcb6cac0a6aadfdc99bdf8bbfca1393da535d4e8945df0648fa95fb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
