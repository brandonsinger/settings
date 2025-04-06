;;; init-tools-misc.el --- Misc tools that could be organized better -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(setq tramp-default-method "ssh")

(midnight-mode +1)

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line))
  )

(use-package flycheck
  :config
  (add-hook 'elpaca-after-init-hook #'global-flycheck-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  )

(use-package amx
  :config
  (amx-mode)
  )

(use-package clipetty
  :diminish
  :hook (elpaca-after-init . global-clipetty-mode))

(use-package devdocs
  :bind
  ("C-h D" . devdocs-lookup)
  )

(use-package eww
  :ensure nil
  :bind
  ("C-c w" . eww)
  :custom
  (eww-auto-rename-buffer "title"))


;;; Need package 'enchant' and ('nuspell' or 'hunspell' or 'aspell')
(use-package jinx
  :after (vertico)
  :diminish
  :hook
  (text-mode-hook . jinx-mode)
  (prog-mode-hook . jinx-mode)
  (conf-mode-hook . jinx-mode)
  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)
  :config
  ;; not quite sure that this multiform thing actually does, but its recommended
  (setq vertico-multiform-commands
        '((jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))
  (vertico-multiform-mode 1))

(use-package free-keys)

(use-package eldoc
  :ensure nil
  :diminish
  )

(use-package git-timemachine
  :commands (git-timemachine))

(use-package vundo
  :commands (vundo))

(provide 'init-tools-misc)
;;; init-tools-misc.el ends here
