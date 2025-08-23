;;; init-treesitter.el --- Get tree-sitter stuff setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(my-need-linux-package "tree-sitter-rust")
(my-need-linux-package "tree-sitter-python")
(my-need-linux-package "tree-sitter-markdown")
(my-need-linux-package "tree-sitter-javascript")

(use-package treesit-auto
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package tree-sitter
  :diminish
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

(provide 'init-treesitter)
;;; init-treesitter.el ends here
