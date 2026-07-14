;;; init-modeline.el --- Mode line setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar my-mode-line-modified
  '(:eval (when (buffer-modified-p) " ●")))
(put 'my-mode-line-modified 'risky-local-variable t)

(defvar my-mode-line-readonly
  '(:eval (when buffer-read-only
            (concat " " (nerd-icons-faicon "nf-fa-lock" :face 'error) " "))))
(put 'my-mode-line-readonly 'risky-local-variable t)

(defvar my-mode-line-remote
  '(:eval (when (and buffer-file-name (file-remote-p buffer-file-name))
            (concat " " (nerd-icons-mdicon "nf-md-server_network" :face 'warning) " "))))
(put 'my-mode-line-remote 'risky-local-variable t)

(defvar my-mode-line-buffer-identification
  '(:eval (propertize "%12b"
                      'face (if (mode-line-window-selected-p)
                                'bold
                              'italic))))
(put 'my-mode-line-buffer-identification 'risky-local-variable t)


;; Taken from Emacs source. Changed ordering and removed options
(setq mode-line-position
      '((line-number-mode
         (column-number-mode
          (column-number-indicator-zero-based
           (10 (:propertize mode-line-position-column-line-format))
           (10 (:propertize (:eval (string-replace "%c" "%C" (car mode-line-position-column-line-format))))))
          (6 (:propertize mode-line-position-line-format)))
         (column-number-mode
          (column-number-indicator-zero-based
           (6 (:propertize mode-line-position-column-format))
           (6 (:propertize (:eval (string-replace "%c" "%C" (car mode-line-position-column-format))))))))
        (:propertize (" " mode-line-percent-position) display (min-width (5.0)))
        ))

(setq-default mode-line-format
              '("%e"
                " 𝝺  "
                my-mode-line-readonly
                my-mode-line-remote
                my-mode-line-buffer-identification
                my-mode-line-modified
                " "
                mode-line-position
                " "
                mode-line-modes
                mode-line-misc-info
                ))

(provide 'init-modeline)
;;; init-modeline.el ends here
