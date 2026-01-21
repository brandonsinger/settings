;;; init-email.el --- Some stuff for email  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(my-need-linux-package "isync") ;; for mbsync
;;(my-need-linux-package "msmtp") ;; for sending emails
(my-need-linux-package "notmuch")

(defun my/notmuch-last-sync ()
  "Get last email sync time from systemd service."
  (let* ((output (shell-command-to-string
                  "systemctl --user show mbsync.service --property=ExecMainExitTimestamp --value"))
         (timestamp (string-trim output)))
    (if (string-empty-p timestamp)
        "Never"
      (let* ((sync-time (date-to-time timestamp))
             (diff (floor (float-time (time-subtract (current-time) sync-time))))
             (minutes (/ diff 60))
             (hours (/ minutes 60))
             (days (/ hours 24))
             (ago (cond
                   ((< diff 60) (format "%d seconds ago" diff))
                   ((< minutes 60) (format "%d minutes ago" minutes))
                   ((< hours 24) (format "%d hours ago" hours))
                   (t (format "%d days ago" days)))))
        (format "%s (%s)" (format-time-string "%b %d, %I:%M %p" sync-time) ago)))))

(defun my/notmuch-insert-last-sync ()
  "Insert last sync time in notmuch hello."
  (widget-insert (format "Last checked: %s  " (my/notmuch-last-sync)))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (my/notmuch-sync-mail)
                           (notmuch-refresh-this-buffer))
                 "Check Now")
  (widget-insert "\n\n"))

(defun my/notmuch-sync-mail ()
  "Trigger email sync via systemd."
  (interactive)
  (message "Syncing mail...")
  (shell-command "systemctl --user start mbsync.service"))

(use-package notmuch
  :ensure nil ; installed via system package manager instead
  :defer 5
  :bind
  ("C-c m" . notmuch)

  :config
  (setq message-kill-buffer-on-exit t)

  (define-key notmuch-hello-mode-map (kbd "G") 'my/notmuch-sync-mail)
  (define-key notmuch-search-mode-map (kbd "G") 'my/notmuch-sync-mail)



  (define-key notmuch-show-mode-map "d"
              (lambda ()
                "toggle deleted tag for message"
                (interactive)
                (if (member "deleted" (notmuch-show-get-tags))
                    (notmuch-show-tag (list "-deleted"))
                  (notmuch-show-tag (list "+deleted")))))
  (define-key notmuch-search-mode-map "d"
              (lambda ()
                "toggle deleted tag for message"
                (interactive)
                (if (member "deleted" (notmuch-search-get-tags))
                    (notmuch-search-tag (list "-deleted"))
                  (notmuch-search-tag (list "+deleted")))))


  (setq notmuch-hello-sections
        '(my/notmuch-insert-last-sync
          notmuch-hello-insert-saved-searches
          notmuch-hello-insert-search
          notmuch-hello-insert-recent-searches
          notmuch-hello-insert-alltags))

  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-show-relative-dates nil)
  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("flag" . italic)))
  )

(provide 'init-email)
;;; init-email.el ends here
