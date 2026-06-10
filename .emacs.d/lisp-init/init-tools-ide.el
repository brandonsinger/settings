;;; init-tools-ide.el --- Sets up IDE like tools -*- lexical-binding: t; -*-
;;; Commentary:

;; Use M-x lsp-doctor to validate if your lsp-mode is properly configured.
;; To install ruff (python linter and code formatter), run: pip install ruff

;;; Code:

(defun echo-install-lsp-servers (server-list)
  "Install specified LSP servers using lsp-install-server. SERVER-LIST is a list of server symbols, e.g. '(pyls tsserver gopls)"
  (dolist (server server-list)
    (if (fboundp 'lsp-install-server)
        (let ((client (gethash server lsp-clients)))
          (when client
            (unless (lsp--server-binary-present? client)
              (lsp-install-server nil server)
              (message "Installed LSP server: %s" server))))
      (error "lsp-install-server function not found. Is lsp-mode installed?")))
  (message "Finished installing LSP servers"))

(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(use-package lsp-mode
  :after corfu
  :init
  (setq lsp-use-plists t)
  :hook
  (
   (css-mode . lsp-deferred)
   (css-ts-mode . lsp-deferred)
   (html-mode . lsp-deferred)
   (html-ts-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (js-ts-mode . lsp-deferred)
   (json-mode . lsp-deferred)
   (json-ts-mode . lsp-deferred)
   (php-mode . lsp-deferred)
   (php-ts-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (python-ts-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   (yaml-ts-mode . lsp-deferred)
   (web-mode . lsp-deferred)
   (dockerfile-mode . lsp-deferred)
   (lsp-completion-mode . my/lsp-mode-setup-completion)
   )
  :commands
  (lsp lsp-deferred)
  :config
  (echo-install-lsp-servers echo-install-lsp-servers-list)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-file-watch-threshold 5000)
  (lsp-completion-provider :none) ;; we use Corfu!
  )

;; TODO: Play around with this to figure out how I want to customize it.
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :bind
  ("<f1>" . lsp-ui-doc-mode)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode))

(use-package company
  :diminish)

(use-package company-ansible
  :after (company)
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package company-web
  :after (company)
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html))
                             (company-mode t))))

(use-package format-all
  :commands format-all-mode)
(add-hook 'sql-mode-hook
          (lambda ()
            (setq format-all-formatters
                  '(("SQL" (sqlformat "-r"))))))

;; TODO: setup some keybindings
(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'consult-xref)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
(with-eval-after-load 'hydra
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("r" dumb-jump-find-references "References")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(provide 'init-tools-ide)
;;; init-tools-ide.el ends here
