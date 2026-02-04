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

  (add-to-list 'treesit-language-source-alist
               '(systemverilog "https://github.com/gmlarumbe/tree-sitter-systemverilog"))
  (add-to-list 'treesit-language-source-alist
               '(verilog "https://github.com/gmlarumbe/tree-sitter-systemverilog"))
  (add-to-list 'treesit-language-source-alist
               '(gitcommit . ("https://github.com/gbprod/tree-sitter-gitcommit")))

  (global-treesit-auto-mode))

;; (use-package tree-sitter-langs)
