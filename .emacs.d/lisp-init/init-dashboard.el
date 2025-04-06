;;; init-dashboard.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dashboard
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  ;;(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
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
