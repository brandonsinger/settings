;;; -*- lexical-binding: t; -*-
;;; Code:

(use-package org
  :ensure nil
  ;; :delight
  :mode ("\\.org\\'" . org-mode)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-startup-indented t)
  (with-eval-after-load 'org-indent
    (require 'diminish)
    (diminish 'org-indent-mode))

  (setq org-ellipsis " ▾"))

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
  :custom
  (verb-auto-kill-response-buffers t)
  )

(provide 'init-org)
;;; init-org.el ends here
