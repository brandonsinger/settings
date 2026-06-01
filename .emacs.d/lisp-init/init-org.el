;;; -*- lexical-binding: t; -*-
;;; Code:

(defun my/project-tasks-file ()
  (concat (projectile-project-root) "tasks.org"))

(use-package org
  :ensure nil
  ;; :delight
  :mode ("\\.org\\'" . org-mode)
  :bind
  ("C-c c" . org-capture)

  :config
  (setq org-capture-templates
        '(("t" "Task" entry
           (file my/project-tasks-file)
           "* TODO %?\n  %U")))

  :custom
  (org-log-into-drawer t)
  (org-ellipsis " ▾")
  (org-startup-indented t)

  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "|" "DONE")))
  )

(use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 4))

;; (defun echo/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))
;; (use-package visual-fill-column-width
;;   :hook (org-mode . echo/org-mode-visual-fill))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))

(use-package verb
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  :custom
  (verb-auto-kill-response-buffers t))


(provide 'init-org)
;;; init-org.el ends here
