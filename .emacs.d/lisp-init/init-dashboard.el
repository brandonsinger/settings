;;; init-dashboard.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dashboard-insert-activities (list-size)
  "Add the list of LIST-SIZE items from activities."
  (dashboard-insert-section
   "Activities:"
   (map-keys activities-activities)
   list-size
   'activities
   "a"
   `(lambda (&rest _) (activities-resume (map-elt activities-activities ',el)))
   (abbreviate-file-name el)))


(use-package dashboard
  :after nerd-icons
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (add-to-list 'dashboard-item-generators '(activities . dashboard-insert-activities))
  ;;(add-to-list 'dashboard-heading-icons '(activities . "nf-oct-project"))
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((activities . 5)
                     (recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (registers . 5)))
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-startupify-list '(dashboard-insert-banner
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

(use-package dashboard-ls
  :disabled
  :after dashboard
  :config
  (add-to-list 'dashboard-items '(ls-directories . 5)))

(use-package votd
  :ensure(:host github :repo "kristjoc/votd")
  :after dashboard
  :config
  (setq dashboard-footer-messages (list (get-votd)))
  :custom
  (votd-bible-version "AMP"))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
