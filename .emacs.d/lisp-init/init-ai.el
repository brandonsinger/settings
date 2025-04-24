;;; init-ai.el --- Setup AI stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package chatgpt-shell
  :commands
  (chatgpt-shell chatgpt-shell-prompt-compose)
  :bind
  ("C-c C-a" . chatgpt-shell-prompt-compose)
  :config
  (chatgpt-shell-ollama-load-models))

(use-package aidermacs
  :disabled
  :bind
  ("C-c a" . aidermacs-transient-menu)
  :custom
  (aidermacs-use-architect-mode t))

(provide 'init-ai)
;;; init-ai.el ends here
