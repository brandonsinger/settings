;;; init-project.el --- Setup project handling -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode ;;TODO: might want to change this
  :config
  (projectile-mode)
  :bind
  ("C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  )

(use-package ripgrep)

(use-package treemacs
  :defer t
  :commands (treemacs))
(use-package treemacs-projectile
  :after treemacs)
(use-package treemacs-tab-bar
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init-project)
;;; init-project.el ends here
