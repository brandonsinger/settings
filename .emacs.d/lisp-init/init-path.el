;;; init-path.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t)))

;; Set backup directory and backup settings
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(provide 'init-path)
;;; init-path.el ends here
