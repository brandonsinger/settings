;; -*- lexical-binding: t; -*-

;;; Things to maybe add
;; - machine.el
;; - emacs-run-command
;; - org-modern?
;; - org-tempo
;; - scrolllock mode?
;; - compile-multi
;; - git-timemachine
;; - pulsar
;; - undo-tree or vundo?
;; - https://karthinks.com/software/simple-folding-with-hideshow/
;; - https://github.com/alphapapa/dogears.el
;; - 'shell' from https://sachachua.com/dotemacs/index.html
;; - how to a scroll different window
;; - rainbow-delimiters
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
(require 'init-ai)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-for-terminal)
(require 'init-interface)
(require 'init-modeline)
(require 'init-modes)
(require 'init-org)
(require 'init-project)
(require 'init-shell)
(require 'init-tool-lsp)
(require 'init-tool-magit)
(require 'init-tools-clean)
(require 'init-tools-misc)
(require 'init-treesitter)
(require 'init-visuals)
(require 'init-window)
(require 'init-workspace)


(when (string= (system-name) "officedev")
  (require 'init-profile-work))
(when (string= (system-name) "echo-bedroom")
  (require 'init-profile-home))

;;; init.el ends here
