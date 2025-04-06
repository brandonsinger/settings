;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Things to maybe add
;; - machine.el
;; - emacs-run-command
;; - org-modern?
;; - org-tempo
;; - scrolllock mode?
;; - compile-multi
;; - pulsar
;; - undo-tree or vundo?
;; - https://karthinks.com/software/simple-folding-with-hideshow/
;; - https://github.com/alphapapa/dogears.el
;; - 'shell' from https://sachachua.com/dotemacs/index.html
;; - how to a scroll different window
;; - flycheck-projectile
;; - maybe term-projectile
;; - selected.el
;; - https://www.masteringemacs.org/article/securely-generating-totp-tokens-emacs
;; - hush
;; - https://github.com/balloneij/selection-highlight-mode
;; - persistent-scratch
;; - https://github.com/TxGVNN/project-tasks
;; - spacious-padding?
;; - sql package?
;; - midnight mode?
;; - switch to project.el? (and maybe use disproject with it?)
;; - atomic-chrome
;; - magit wip mode?
;; - might want to go through https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org again
;; - magit-gptcommit
;; - consult has many more feature that I dont use
;; - dwim-shell-command
;; - maybe https://github.com/progfolio/elpaca/wiki/Automatic-Rebuilding-of-Packages-on-Restart

;; - use occur, make note somewhere?


;;; Code:

(add-to-list 'load-path (expand-file-name "lisp-init" user-emacs-directory))

(require 'init-basic)
(require 'init-path)
(require 'init-elpaca)
(require 'init-theme)
;; these can be in any order
(let ((debug-on-error nil))
  (with-demoted-errors "Error: %S" (require 'init-ai))
  (with-demoted-errors "Error: %S" (require 'init-dashboard))
  (with-demoted-errors "Error: %S" (require 'init-dired))
  (with-demoted-errors "Error: %S" (require 'init-for-terminal))
  (with-demoted-errors "Error: %S" (require 'init-interface))
  (with-demoted-errors "Error: %S" (require 'init-keybinds))
  (with-demoted-errors "Error: %S" (require 'init-modeline))
  (with-demoted-errors "Error: %S" (require 'init-modes))
  (with-demoted-errors "Error: %S" (require 'init-org))
  (with-demoted-errors "Error: %S" (require 'init-project))
  (with-demoted-errors "Error: %S" (require 'init-shell))
  (with-demoted-errors "Error: %S" (require 'init-tool-lsp))
  (with-demoted-errors "Error: %S" (require 'init-tool-magit))
  (with-demoted-errors "Error: %S" (require 'init-tools-clean))
  (with-demoted-errors "Error: %S" (require 'init-tools-misc))
  (with-demoted-errors "Error: %S" (require 'init-treesitter))
  (with-demoted-errors "Error: %S" (require 'init-visuals))
  (with-demoted-errors "Error: %S" (require 'init-window))
  (with-demoted-errors "Error: %S" (require 'init-workspace))
  )


(when (string= (system-name) "officedev")
  (require 'init-profile-work))
(when (string= (system-name) "echo-bedroom")
  (require 'init-profile-home))

;;; init.el ends here
