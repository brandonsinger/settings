;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))

(global-set-key (kbd "C-x s") 'save-buffer)


(use-package selected
  :diminish
  :bind (:map selected-keymap
              ("l" . sort-lines)
              ("w" . count-words-region))
  :config
  (selected-global-mode 1))

;; TODO: (bind-key "????" dabbrev-completion) what should i use?

;; TODO: something(s) for changing current tab

(provide 'init-keybinds)
;;; init-keybinds.el ends here
