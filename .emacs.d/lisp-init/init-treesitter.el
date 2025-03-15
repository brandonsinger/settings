;;; init-treesitter.el --- Get tree-sitter stuff setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 5)
  (treesit-auto-install 'prompt)
  (treesit-language-source-alist
   '(
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ))
  (treesit-auto-langs '(javascript yaml json html css elisp php rust toml))
  :config
  (global-treesit-auto-mode))

(use-package tree-sitter
  :diminish
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

(provide 'init-treesitter)
;;; init-treesitter.el ends here
