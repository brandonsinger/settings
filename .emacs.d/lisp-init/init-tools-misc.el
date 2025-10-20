;;; init-tools-misc.el --- Misc tools that could be organized better -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./,
(setq tramp-default-method "ssh")
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)
(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)
(setq magit-tramp-pipe-stty-settings 'pty)
(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(midnight-mode +1)

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line))
  )

(use-package flycheck
  :config
  (add-hook 'elpaca-after-init-hook #'global-flycheck-mode))

(use-package flycheck-rust
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; TODO: configure it more
(use-package flyover
  :after flycheck
  :init
  ;;(add-hook 'flycheck-mode-hook #'flyover-mode)
  )

;; TODO: might remove this one
(use-package flycheck-indicator
  :after (flycheck)
  :hook (flycheck-mode . flycheck-indicator-mode))

;; TODO: add to the projectile-command-map too
(use-package flycheck-projectile
  :commands (flycheck-projectile-list-errors))

(use-package amx
  :config
  (amx-mode))

(use-package clipetty
  :diminish
  :hook (elpaca-after-init . global-clipetty-mode))

(use-package devdocs
  :bind
  ("C-h D" . devdocs-lookup))
(use-package devdocs-browser
  :custom
  (devdocs-data-dir (expand-file-name  "var/devdocs-browser"  user-emacs-directory))
  (devdocs-browser-cache-directory (expand-file-name  "var/devdocs-browser/cache"  user-emacs-directory))
  (devdocs-browser-data-directory (expand-file-name  "var/devdocs-browser/data" user-emacs-directory))
  :hook
  (php-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("php"))))
  (rust-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("rust"))))
  (js-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("javascript"))))
  (html-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("html"))))
  )

(use-package eww
  :ensure nil
  :bind
  ("C-c w" . eww)
  :custom
  (eww-auto-rename-buffer "title"))


;;; Need package 'enchant' and ('nuspell' or 'hunspell' or 'aspell')
(my-need-linux-package "enchant")
(use-package jinx
  :after (vertico)
  :diminish
  :hook
  (text-mode-hook . jinx-mode)
  (prog-mode-hook . jinx-mode)
  (conf-mode-hook . jinx-mode)
  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)
  :config
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-commands
   '((jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))))

(use-package free-keys
  :commands (free-keys))

;; Most of this from https://github.com/rileyrg/Emacs-Customisations?tab=readme-ov-file#eldoc
(use-package eldoc
  :ensure nil
  :diminish
  :bind
  ("C-." . rgr/eldoc-at-point)
  :config
  (global-eldoc-mode)
  (defun rgr/eldoc-at-point()
    (interactive)
    (if eldoc-mode
        (eldoc-box-help-at-point)
      (message "eldoc not active"))))
(use-package eldoc-box
  :after eldoc)


(use-package vundo
  :bind
  (("C-x u". vundo)))

(use-package goggles
  :diminish
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
(defun my/pulse-current-region (&rest _)
  "Pulse the current implicit or active region."
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))
(advice-add #'kill-ring-save :before #'my/pulse-current-region)

(use-package su
  :config
  (su-mode +1))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h o" . helpful-at-point))

(use-package proced
  :ensure nil
  :custom
  (proced-enable-color-flag t)
  (proced-auto-update-flag 'visible "Update only visible buffers")
  (proced-tree-flag t))

;; TODO: add persistant-scratch?

(use-package daemons
  :commands (daemons))

(provide 'init-tools-misc)
;;; init-tools-misc.el ends here
