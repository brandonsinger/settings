;;; init-theme.el --- Theme stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ef-themes
  :demand t
  :init
  ;;(ef-themes-take-over-modus-themes-mode 1)
  (setq modus-themes-to-toggle '(ef-elea-dark ef-eagle))

  (setq modus-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))
  ;; Load the theme of choice:
  :config
  (modus-themes-load-theme 'ef-elea-dark)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  :bind
  ("<f5>" . ef-themes-toggle))

(provide 'init-theme)
;;; init-theme.el ends here
