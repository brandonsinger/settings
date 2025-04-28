;;; init-modes.el --- Setup stuff for modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.php[s34]?\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.html.j2\\'" . web-mode))
  :config
  (setopt web-mode-enable-auto-indentation nil))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(setq web-mode-engines-alist
      '(
        ("django" . "/home/echo/projects/website/templates/.*\\.twig.html\\'")
        ("smarty" . "/home/echo/projects/website/templates/.*\\.html\\'")
        )
      )

(use-package rust-mode
  :init
  ;;(setq rust-mode-treesitter-derive t)
  :mode ("\\.rs\\'" . rust-mode))
(use-package rustic
  :after (rust-mode)
  :diminish
  :config
  (setq rustic-format-on-save t))


(use-package csv-mode
  :mode ("\\.csv$" . csv-mode))

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" . conf-mode))

(my-need-linux-package "prettier")
(use-package prettier
  :ensure t
  :hook ((css-mode
          js2-mode
          html-mode
          json-mode
          typescript-mode) . prettier-mode))

;; (use-package python-pytest)
;; (global-set-key (kbd "C-x T") 'python-pytest-dispatch)


;; would be nice to wrap this in a use-package somehow...
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)

(use-package ssh-config-mode
  :defer t)

(provide 'init-modes)
;;; init-modes.el ends here
