;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))

;; (bind-key "????" dabbrev-completion) what should i use?

;; TODO: something(s) for changing current tab

(provide 'init-keybinds)
;;; init-keybinds.el ends here
