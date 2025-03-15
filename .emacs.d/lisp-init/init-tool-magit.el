;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;;  use ~magit-list-repositories~ to get a status list of all projects
;;; Code:

(use-package transient)

(use-package magit
  :after (transient)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories '(("~/projects" . 1)))
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident ())
          ("Version" 25 magit-repolist-column-version ())
          ("D"        1 magit-repolist-column-flag ())
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U"      3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path"    99 magit-repolist-column-path ())))
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  )

(use-package magit-todos
  :init
  (magit-todos-mode)
  )

;;(use-package git-timemachine)

(provide 'init-tool-magit)
;;; init-tool-magit.el ends here
