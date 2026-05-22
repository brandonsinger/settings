;;; init-project.el --- Setup project handling -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :demand
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("C-x p" . projectile-command-map)
  ("s-p" . projectile-command-map))

(use-package consult-projectile
  :after (projectile)
  :bind
  (:map projectile-command-map
        ("f" . consult-projectile)))

(use-package ripgrep)

(use-package treemacs
  :defer t
  :commands (treemacs))
(use-package treemacs-projectile
  :after treemacs)
(use-package treemacs-tab-bar
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons))

(use-package dir-config
  :diminish
  :custom
  (dir-config-file-names '(".dir-config.el"))
  (dir-config-allowed-directories '("~/projects"))
  :config
  (dir-config-mode))

(provide 'init-project)
;;; init-project.el ends here
