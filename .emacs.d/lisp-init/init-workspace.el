;;; init-workspace.el --- Setup for what I use for 'workspaces' -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package easysession
  :diminish easysession-save-mode
  :custom
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'elpaca-after-init-hook #'easysession-load-including-geometry 98)
  (add-hook 'elpaca-after-init-hook #'easysession-save-mode 99))

(defun my/new-tab-with-scratch (tab-name)
  "Create a new tab named TAB-NAME with *scratch* buffer."
  (interactive "sTab name: ")
  (tab-new)
  (tab-rename tab-name)
  (switch-to-buffer "*scratch*"))
(defun my/projectile-new-tab-scratch ()
  "Select a projectile project, open it in a new tab, and switch to *scratch* buffer."
  (interactive)
  (let* ((project (completing-read "Switch to project: "
                                   projectile-known-projects))
         (project-name (file-name-nondirectory (directory-file-name project))))
    (tab-bar-new-tab)
    (tab-bar-rename-tab project-name)
    (let ((default-directory project))
      (switch-to-buffer "*scratch*"))))

(use-package tab-bar
  :ensure nil
  :bind
  (
   ("C-x t <left>" . tab-bar-switch-to-prev-tab)
   ("C-x t <right>" . tab-bar-switch-to-next-tab)
   ("C-x t R" . tab-bar-rename-tab)
   ("C-x t K" . tab-bar-close-tab)
   ("C-x t N" . my/new-tab-with-scratch)
   ("C-x t P" . my/projectile-new-tab-scratch)
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
