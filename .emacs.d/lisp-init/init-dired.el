;;; init-dired.el --- Setup dired -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
  :init
  (setq dired-auto-revert-buffer t)
  )
(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(provide 'init-dired)
;;; init-dired.el ends here
