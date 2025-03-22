;;; -*- lexical-binding: t; -*-
;;; Code:

(use-package ef-themes
  :demand t
  ;;:after (org)
  :init
  (setq ef-themes-to-toggle '(ef-bio ef-duo-light))


  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  (setq ef-themes-region '(no-extend))

  ;; ;; not working, must be doing something wrong
  ;; (setq ef-bio-palette-overrides
  ;;       '((cursor red)
  ;;         (org-blocks green))
  ;;       )


  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  :config
  (load-theme 'ef-bio :no-confirm)
  :bind
  ("<f5>" . ef-themes-toggle))

(provide 'init-theme)
;;; init-theme.el ends here
