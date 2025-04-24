;;; init-profile-work.el --- Some setup for when at work  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; set the same env as activate_env.sh
(setenv "ANSIBLE_VAULT_PASSWORD_FILE" "lpass_vault.sh")
(setenv "ANSIBLE_PIPELINING" nil)
(setenv "LPASS_DISABLE_PINENTRY" nil)
(setenv "LPASS_PINENTRY" "~/tools/pinentry-emacs") ;; need to manually install pinentry-emacs

;; eww, works but is too slow
;; (setq projectile-indexing-method 'native)


(custom-set-variables
 '(safe-local-variable-values '((magit-todos-exclude-globs "htdocs_wp/*"))))

(setq echo-install-lsp-servers-list `(ansible-ls html-ls js-ls json-ls css-ls iph))

;; TODO: try out phpunit.el

(message "'Work' system changes loaded")

(provide 'init-profile-work)
;;; init-profile-work.el ends here
