;;; -*- lexical-binding: t; -*-
;;; Code:

(setq visible-bell t)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook)
              )
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package tab-line
  :ensure nil
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                          'keymap tab-line-right-map
                                          'mouse-face 'tab-line-highlight
                                          'help-echo "Click to scroll right")
        tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                         'keymap tab-line-left-map
                                         'mouse-face 'tab-line-highlight
                                         'help-echo "Click to scroll left"))
  :custom-face
  (activities-tabs ((t (:height 1.2))))
  (mode-line ((t (:height 1.1))))
  )

(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))
(when (member "RobotoMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "RobotoMono Nerd Font"))

(global-hl-line-mode 1)
(use-package lin
  :hook
  (elpaca-after-init . lin-global-mode)
  :config
  (setq lin-face 'lin-blue))


;; To make this setup work, the user must type M-x and then call the command nerd-icons-install-fonts.
(use-package nerd-icons
  )

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after marginalia
  ;; FIXME 2024-09-01: For some reason this stopped working because it
  ;; macroexpands to `marginalia-mode' instead of
  ;; `marginalia-mode-hook'.  What is more puzzling is that this does
  ;; not happen in the next :hook...
  ;; :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package rainbow-mode
  :hook
  (web-mode-hook . rainbow-mode)
  (css-mode-hook . rainbow-mode)
  (html-mode-hook . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t))
  )

(provide 'init-visuals)
;;; init-visuals.el ends here
