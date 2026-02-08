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
  (setopt web-mode-enable-auto-indentation nil)
  ;; Remove after web-mode loads
  (setq auto-mode-alist (rassq-delete-all 'mhtml-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all 'html-ts-mode auto-mode-alist))
  )

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
  (setq rust-mode-treesitter-derive t))
(use-package rustic
  :after (rust-mode,tree-sitter)
  :diminish
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save t)
  (setq auto-mode-alist
        (seq-remove (lambda (entry)
                      (memq (cdr entry) '(rust-mode rust-ts-mode)))
                    auto-mode-alist))
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (setq auto-mode-alist
                    (seq-remove (lambda (entry)
                                  (memq (cdr entry) '(rust-mode rust-ts-mode)))
                                auto-mode-alist))
              ))
  :custom
  (rustic-cargo-use-last-stored-arguments t))


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

;; TODO: update sql mode to use 4 space 'tabs'
(my-need-linux-package "pgformatter")
(use-package sqlformat
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s4" "--no-space-function")))

;; (use-package python-pytest)
;; (global-set-key (kbd "C-x T") 'python-pytest-dispatch)


;; would be nice to wrap this in a use-package somehow...
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)

(use-package ssh-config-mode
  :defer t)

(use-package systemd)

(provide 'init-modes)
;;; init-modes.el ends here
