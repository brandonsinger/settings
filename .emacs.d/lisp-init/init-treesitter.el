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
  :diminish
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
  (treesit-fold-indicators-mode " ▾")
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
(add-hook 'php-ts-mode-hook #'treesit-fold-mode)
(add-hook 'csharp-ts-mode-hook #'treesit-fold-mode)
(add-hook 'go-mod-ts-mode-hook #'treesit-fold-mode)
(add-hook 'lua-ts-mode-hook #'treesit-fold-mode)

;; Web and Frontend
(add-hook 'js-ts-mode-hook #'treesit-fold-mode)
(add-hook 'typescript-ts-mode-hook #'treesit-fold-mode)
(add-hook 'tsx-ts-mode-hook #'treesit-fold-mode)
(add-hook 'css-ts-mode-hook #'treesit-fold-mode)
(add-hook 'html-ts-mode-hook #'treesit-fold-mode)
(add-hook 'heex-ts-mode-hook #'treesit-fold-mode)
(add-hook 'xml-ts-mode-hook #'treesit-fold-mode)

;; Scripting and Infrastructure
(add-hook 'bash-ts-mode-hook #'treesit-fold-mode)
(add-hook 'cmake-ts-mode-hook #'treesit-fold-mode)
(add-hook 'dockerfile-ts-mode-hook #'treesit-fold-mode)

;; Data and Configuration
(add-hook 'json-ts-mode-hook #'treesit-fold-mode)
(add-hook 'toml-ts-mode-hook #'treesit-fold-mode)

;; Build Systems and Makefiles
(add-hook 'makefile-ts-mode-hook #'treesit-fold-mode)

;; Hardware Description and Shaders
(add-hook 'verilog-ts-mode-hook #'treesit-fold-mode)
(add-hook 'vhdl-ts-mode-hook #'treesit-fold-mode)
(add-hook 'hlsl-ts-mode-hook #'treesit-fold-mode)

;; Documentation and Diagrams
(add-hook 'markdown-ts-mode-hook #'treesit-fold-mode)
(add-hook 'mermaid-ts-mode-hook #'treesit-fold-mode)

;; Other
(add-hook 'gdscript-ts-mode-hook #'treesit-fold-mode)
(add-hook 'clojure-ts-mode-hook #'treesit-fold-mode)
(add-hook 'caml-ts-mode-hook #'treesit-fold-mode)
(add-hook 'ocaml-ts-mode-hook #'treesit-fold-mode)
(add-hook 'erlang-ts-mode-hook #'treesit-fold-mode)
(add-hook 'elixir-ts-mode-hook #'treesit-fold-mode)
(add-hook 'scala-ts-mode-hook #'treesit-fold-mode)
(add-hook 'dart-ts-mode-hook #'treesit-fold-mode)
(add-hook 'haskell-ts-mode-hook #'treesit-fold-mode)
(add-hook 'julia-ts-mode-hook #'treesit-fold-mode)
(add-hook 'kotlin-ts-mode-hook #'treesit-fold-mode)
(add-hook 'gleam-ts-mode-hook #'treesit-fold-mode)
(add-hook 'noir-ts-mode-hook #'treesit-fold-mode)
(add-hook 'swift-ts-mode-hook #'treesit-fold-mode)
(add-hook 'zig-ts-mode-hook #'treesit-fold-mode)

;; TODO: play with this more
(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

;; (use-package tree-sitter-langs)
(provide 'init-treesitter)
;;; init-treesitter.el ends here
