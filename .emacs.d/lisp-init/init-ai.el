;;; init-ai.el --- Setup AI stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: add binding to clear buffer
(use-package gptel
  :bind
  ("C-c a" . gptel-menu)
  ("C-c C-a" . gptel)
  :config
  (add-hook 'gptel-mode-hook (lambda () (setq truncate-lines nil)))
  :custom
  (gptel-default-mode 'org-mode))

(use-package gptel-magit
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install)
  :config
  (setq gptel-magit-prompt-template
        "Write a conventional commit message for these changes.
Format: <type>: <description>
Types: feat, fix, docs, style, refactor, perf, test, chore
Rules: imperative mood, under 72 chars, no period
Only output the message.

Changes:
%s"))

(provide 'init-ai)
;;; init-ai.el ends here
