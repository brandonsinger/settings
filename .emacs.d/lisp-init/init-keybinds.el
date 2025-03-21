;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-

(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))

(provide 'init-keybinds)
;;; init-keybinds.el ends here
