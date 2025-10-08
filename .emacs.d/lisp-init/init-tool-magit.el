;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;;  use ~magit-list-repositories~ to get a status list of all projects
;;; Code:

(use-package transient)
(use-package cond-let)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-repository-directories '(("~/projects" . 1)))
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-save-repository-buffers 'dontask)
  (magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))
  (magit-repolist-columns
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
  )

;; TODO: test.
;;TODO: test2.
;;; TODO: asdf.
                                        ; TODO: asdf2.
;; TODO: Foo bar.

(use-package magit-todos
  :after (magit)
  :config (magit-todos-mode 1))

(use-package git-timemachine
  :commands (git-timemachine))

(provide 'init-tool-magit)
;;; init-tool-magit.el ends here
