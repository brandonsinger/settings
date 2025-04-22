;;; init-tool-lsp.el --- Sets up IDE like tools -*- lexical-binding: t; -*-
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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-file-watch-threshold 5000
        lsp-use-plists t)
  :hook (
         (web-mode . lsp)
         (css-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (echo-install-lsp-servers echo-install-lsp-servers-list)
  )

;; TODO: Play around with this to figure out how I want to customize it.
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :bind
  ("<f1>" . lsp-ui-doc-mode)
  )

(use-package yasnippet
  :diminish
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

(provide 'init-tool-lsp)
;;; init-tool-lsp.el ends here
