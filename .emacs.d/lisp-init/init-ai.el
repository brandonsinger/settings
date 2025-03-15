;;; init-ai.el --- Setup AI stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package shell-maker
  ;;:straight (:type git :host github :repo "xenodium/shell-maker" :files ("shell-maker*.el"))
  )

(use-package chatgpt-shell
  ;;:straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  ;;:custom
  ;; ((chatgpt-shell-anthropic-key
  ;;   (lambda ()
  ;;     (auth-source-pass-get 'secret "openai-key"))))
  )

(provide 'init-ai)
;;; init-ai.el ends here
