;;; init-tools-clean.el --- Some tools for keeps code clean -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ws-butler
  :diminish
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
         (css-mode-hook . aggressive-indent-mode)))

(provide 'init-tools-clean)
;;; init-tools-clean.el ends here
