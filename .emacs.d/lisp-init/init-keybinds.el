;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))

(global-set-key (kbd "C-x s") 'save-buffer)

(global-set-key (kbd "C-x k") #'bury-buffer)
(global-set-key (kbd "C-x K") #'kill-current-buffer)

(global-set-key (kbd "C-D") #'duplicate-dwim)

(use-package selected
  :diminish
  :bind (:map selected-keymap
              ("l" . sort-lines)
              ("w" . count-words-region))
  :config
  (selected-global-mode 1))

;; TODO: something(s) for changing current tab

(provide 'init-keybinds)
;;; init-keybinds.el ends here
