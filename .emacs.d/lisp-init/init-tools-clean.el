;;; init-tools-clean.el --- Some tools for keeps code clean -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package whitespace-cleanup-mode
  :diminish
  :config
  (global-whitespace-cleanup-mode t))

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
         (css-mode-hook . aggressive-indent-mode)))

(provide 'init-tools-clean)
;;; init-tools-clean.el ends here
