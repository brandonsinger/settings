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


(use-package treesit-fold
  :commands (treesit-fold-close
             treesit-fold-close-all
             treesit-fold-open
             treesit-fold-toggle
             treesit-fold-open-all
             treesit-fold-mode
             global-treesit-fold-mode
             treesit-fold-open-recursively
             treesit-fold-line-comment-mode)
  :custom
  (treesit-fold-line-count-show t)
  (treesit-fold-line-count-format " ▼")
  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold))

;; Systems and General Purpose
(add-hook 'c-ts-mode-hook #'treesit-fold-mode)
(add-hook 'c++-ts-mode-hook #'treesit-fold-mode)
(add-hook 'java-ts-mode-hook #'treesit-fold-mode)
(add-hook 'rust-ts-mode-hook #'treesit-fold-mode)
(add-hook 'go-ts-mode-hook #'treesit-fold-mode)
(add-hook 'ruby-ts-mode-hook #'treesit-fold-mode)

;; Web and Frontend
(add-hook 'js-ts-mode-hook #'treesit-fold-mode)
(add-hook 'typescript-ts-mode-hook #'treesit-fold-mode)
(add-hook 'tsx-ts-mode-hook #'treesit-fold-mode)
(add-hook 'css-ts-mode-hook #'treesit-fold-mode)
(add-hook 'html-ts-mode-hook #'treesit-fold-mode)

;; Scripting and Infrastructure
(add-hook 'bash-ts-mode-hook #'treesit-fold-mode)
(add-hook 'cmake-ts-mode-hook #'treesit-fold-mode)
(add-hook 'dockerfile-ts-mode-hook #'treesit-fold-mode)

;; Data and Configuration
(add-hook 'json-ts-mode-hook #'treesit-fold-mode)
(add-hook 'toml-ts-mode-hook #'treesit-fold-mode)


;; (use-package tree-sitter-langs)
(provide 'init-treesitter)
;;; init-treesitter.el ends here
