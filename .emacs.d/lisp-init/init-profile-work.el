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

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))


;; TODO: need a better way, I shouldn't need to do this
(custom-set-variables
 '(safe-local-variable-values '((magit-todos-exclude-globs "htdocs_wp/*" "js/other*" "lib/others/*"
                                                           "lib/smarty/*" "lib/HTMLPurifier/*"
                                                           "lib/google-api-php-client/*"))))

(use-package ansible
  :hook (yaml-mode . ansible))
(use-package ansible-doc
  :hook (yaml-mode . ansible-doc-mode))

(setq echo-install-lsp-servers-list `(ansible-ls html-ls js-ls json-ls css-ls iph))

;; TODO: try out phpunit.el
;; TODO: use purple-rocketchat to libpurple to BitlBee to IRC (use ERC?)

(message "'Work' system changes loaded")

(provide 'init-profile-work)
;;; init-profile-work.el ends here
