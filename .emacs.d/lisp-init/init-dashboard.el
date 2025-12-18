;;; init-dashboard.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun echo/dashboard-insert-activities (list-size)
  "Add the list of LIST-SIZE items from activities."
  (dashboard-insert-section
   "Activities:"
   (let ((pairs (cl-loop for (key . activity) in activities-activities
                         for last-state = (activities-activity-last activity)
                         for time-val = (when last-state
                                          (map-elt (activities-activity-state-etc last-state) 'time))
                         when time-val
                         collect (cons key time-val))))
     (mapcar #'car (sort pairs (lambda (a b)
                                 (or (> (nth 0 (cdr a)) (nth 0 (cdr b)))
                                     (and (= (nth 0 (cdr a)) (nth 0 (cdr b)))
                                          (> (nth 1 (cdr a)) (nth 1 (cdr b)))))))))
   list-size
   'activities
   "a"
   `(lambda (&rest _) (activities-resume (map-elt activities-activities ',el)))
   (abbreviate-file-name el)))

;; TODO: add agenda when after I finally set that up
(use-package dashboard
  :after nerd-icons
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (add-to-list 'dashboard-item-generators '(activities . echo/dashboard-insert-activities))
  (add-to-list 'dashboard-heading-icons '(activities . "nf-oct-project"))
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '(
                     (activities . 15)
                     (projects  . 5)
                     (recents   . 5)
                     (bookmarks . 1)
                     (agenda    . 1)
                     (registers . 1)))
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
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
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))) ;; for showing Dashboard in frames created with emacsclient -c
  )

(use-package bible-gateway
  :ensure t
  :after dashboard
  :config
  (let ((verse (bible-gateway-get-verse)))
    (setq dashboard-footer-messages (list verse))
    (setq initial-scratch-message
          (concat ";;; *scratch* ;;;\n\n"
                  (string-join
                   (mapcar (lambda (line) (concat ";;; " line))
                           (split-string verse "\n"))
                   "\n")
                  "\n\n")))
  :custom
  (bible-gateway-include-ref t)
  (bible-gateway-bible-version "AMP"))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
