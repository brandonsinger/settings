;;; init-dashboard.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dashboard
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setopt dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setopt dashboard-startup-banner 'logo)
  (setopt dashboard-projects-backend 'projectile)
  (setopt dashboard-items '((recents   . 5)
                            (bookmarks . 5)
                            (projects  . 5)
                            (agenda    . 5)
                            (registers . 5)))
  (setopt dashboard-display-icons-p t)
  (setopt dashboard-icon-type 'nerd-icons)
  (setopt dashboard-set-file-icons t)
  (setopt dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-newline
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-newline
                                      dashboard-insert-footer))
  ;;(setopt dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  )

(use-package votd
  :ensure(:host github :repo "kristjoc/votd")
  :after dashboard
  :config
  (setq dashboard-footer-messages (list (get-votd)))
  :custom
  (votd-bible-version "AMP"))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
