;;; init-tools-misc.el --- Misc tools that could be organized better -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(setq tramp-default-method "ssh")


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
  )

;; M-$ is ispell-word by default, should change it to something better..?
;; Make sure aspell is installed and setup. (install aspell and aspell-us)
(use-package wucuo
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
  (setq wucuo-spell-check-buffer-predicate
        (lambda ()
          (not (memq major-mode '(dired-mode
                                  log-edit-mode
                                  compilation-mode
                                  help-mode
                                  profiler-report-mode
                                  speedbar-mode
                                  gud-mode
                                  calc-mode
                                  Info-mode)))))

  :hook
  (prog-mode . wucuo-start)
  (text-mode . wucuo-start)
  )

(use-package free-keys)

(use-package eldoc
  :ensure nil
  :diminish
  )

(provide 'init-tools-misc)
;;; init-tools-misc.el ends here
