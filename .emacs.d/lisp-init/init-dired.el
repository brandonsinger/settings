;;; init-dired.el --- Setup dired -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-alh --group-directories-first --time-style=long-iso")
  :init
  (setopt dired-auto-revert-buffer t)
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
  (setopt dired-subtree-use-backgrounds nil))

;; https://web.archive.org/web/20181107114116/http://iqbalansari.me/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)



(provide 'init-dired)
;;; init-dired.el ends here
