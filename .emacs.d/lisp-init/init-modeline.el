;;; init-modeline.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt mode-line-format
        '("%e"
          (:propertize
           ("" mode-line-mule-info mode-line-client mode-line-modified
            mode-line-remote)
           display (min-width (5.0)))
          " "
          mode-line-buffer-identification
          mode-line-position
          (vc-mode vc-mode)
          mode-line-modes
          mode-line-misc-info
          ))

;;use mode-line-format-right-align, new in 30.1
(use-package time
  :ensure nil
  :config
  (setopt display-time-interval 60)
  (setopt display-time-default-load-average nil)
  (display-time)
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces (list (propertize " "
                                               'display '(space :align-to (- right 6)))
                                   'display-time-string))
  )

(provide 'init-modeline)
;;; init-modeline.el ends here
