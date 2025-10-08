;;; init-workspace.el --- Setup for what I use for 'workspaces' -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package easysession
  :custom
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'elpaca-after-init-hook #'easysession-load-including-geometry 98)
  (add-hook 'elpaca-after-init-hook #'easysession-save-mode 99))

(use-package tab-bar
  :ensure nil
  :bind
  (
   ("C-x t <left>" . tab-bar-switch-to-prev-tab)
   ("C-x t <right>" . tab-bar-switch-to-next-tab)
   ("C-x t R" . tab-bar-rename-tab)
   ("C-x t K" . tab-bar-close-tab)
   ("C-x t N" . tab-bar-new-tab)
   ))

;;; TODO: add Filter saved bookmark buffers stuff?
(use-package bufferlo
  :init
  (setq bufferlo-bookmarks-save-at-emacs-exit 'all)
  (setq bufferlo-bookmarks-load-at-emacs-startup 'all)
  (bufferlo-mode)
  :bind
  (
   ("C-x b" . bufferlo-switch-to-buffer)
   ("C-x C-b" . bufferlo-switch-to-buffer)
   ("C-x k" . bufferlo-remove) ;; or bufferlo-bury?
   )
  :config
  (setopt bufferlo-bookmarks-auto-save-interval 120)
  (setq bufferlo-bookmark-frame-save-on-delete 'if-current)
  (setq bufferlo-bookmark-tab-save-on-close 'if-current))

(provide 'init-workspace)
;;; init-workspace.el ends here
