;;; init-ai.el --- Setup AI stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package chatgpt-shell
  :commands
  (chatgpt-shell chatgpt-shell-prompt-compose)
  :bind
  ("C-c C-a" . chatgpt-shell-prompt-compose)
  ("C-c a" . chatgpt-shell)
  :config
  (chatgpt-shell-ollama-load-models)
  )

(provide 'init-ai)
;;; init-ai.el ends here
