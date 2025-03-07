;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)

(setq-default tab-width 4)

(column-number-mode)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
;; mode.  Vertico commands are hidden in normal buffers. This setting is
;; useful beyond Vertico.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq load-prefer-newer t)

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Set the right directory to store the native comp cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
  ;; Ensure JIT compilation is enabled for improved performance by
  ;; native-compiling loaded .elc files asynchronously
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1
  )

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(customize-set-variable 'kill-do-not-save-duplicates t)

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

(setopt enable-recursive-minibuffers t)
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completion-auto-select 'second-tab)

;;(setq bookmark-save-flag 1)
(setq sentence-end-double-space nil)
(setq delete-selection-mode t)
(setq global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq man-notify-method 'aggressive)
(setq confirm-kill-emacs #'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x <") 'org-insert-structure-template)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; From https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;;todo: use instead? (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

(use-package emacs
  :config
  (defun meain/set-read-only-if-do-not-edit ()
    "Set the buffer to read-only if buffer contents has 'DO NOT EDIT' in it.
We limit the search to just top 10 lines so as to only check the header."
    (save-excursion
      (goto-char (point-min))
      (let ((content
             (buffer-substring (point)
                               (save-excursion (forward-line 10) (point)))))
        (when (and (not buffer-read-only)
                   (string-match "DO NOT EDIT" content))
          (read-only-mode 1)
          (message "Buffer seems to be generated. Set to read-only mode.")))))
  (add-hook 'find-file-hook 'meain/set-read-only-if-do-not-edit))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
        (elpaca-use-package-mode)
        (setq use-package-always-ensure t)
        )
(use-package diminish :ensure (:wait t) :demand t)
(use-package delight :ensure (:wait t) :demand t)

(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package no-littering)

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  (setq activities-bookmark-store t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)
   ("C-X C-a <DELETE>" . activities-discard)
   ;;("C-x b" . activities-switch-buffer)
   ))

(setq visible-bell t)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook)
              )
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode 1)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; TODO: shouldnt eldoc be in a different section? and maybe I should actually use this
(use-package eldoc
  :ensure nil
  :diminish
  )

(use-package ef-themes
  :demand t
  :after (org)
  :init
  (setq ef-themes-to-toggle '(ef-bio ef-duo-light))


  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  (setq ef-themes-region '(no-extend))

  ;; not working, must be doing something wrong
  (setq ef-bio-palette-overrides
        '((cursor red)
          (org-blocks green))
        )


  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  :config
  (load-theme 'ef-bio :no-confirm)
  :bind
  ("<f5>" . ef-themes-toggle)
  )

;; To make this setup work, the user must type M-x and then call the command nerd-icons-install-fonts.
(use-package nerd-icons
  )

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after marginalia
  ;; FIXME 2024-09-01: For some reason this stopped working because it
  ;; macroexpands to `marginalia-mode' instead of
  ;; `marginalia-mode-hook'.  What is more puzzling is that this does
  ;; not happen in the next :hook...
  ;; :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; (use-package prism
;;   )

(use-package lin
  :hook
  (elpaca-after-init . lin-global-mode)
  :config
  (setq lin-face 'lin-blue))

(use-package org
  :ensure nil
  ;; :delight
  :config
  (setq org-startup-indented t)
  (with-eval-after-load 'org-indent
    (require 'diminish)
    (diminish 'org-indent-mode))

  (setq org-ellipsis " ▾")
  ;; (setq org-agenda-files '("~/projects/gtd/inbox.org"
  ;;                          "~/projects/gtd/gtd.org"
  ;;                          "~/projects/gtd/tickler.org"))
  ;; (setq org-capture-templates
  ;;       '(
  ;;         ("t" "Todo [inbox]" entry
  ;;          (file+headline "~/projects/gtd/inbox.org" "Tasks")
  ;;          "* TODO %i%?")
  ;;         ("T" "Tickler" entry
  ;;          (file+headline "~/projects/gtd/tickler.org" "Tickler")
  ;;          "* %i%? \n %U")
  ;;         ))
  ;; (setq org-refile-targets '(("~/projects/gtd/gtd.org" :maxlevel . 3)
  ;;                            ("~/projects/gtd/someday.org" :level . 1)
  ;;                            ("~/projects/gtd/tickler.org" :maxlevel . 2)))
  ;; (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; (setq-default org-enforce-todo-dependencies t)
  ;; (setq org-refile-use-outline-path 'file)
  ;; (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-tag-alist '((:startgroup . nil)
  ;;                       ("@work" . ?w)
  ;;                       ("@home" . ?h)
  ;;                       ("@computer" . ?c)
  ;;                       (:endgroup . nil)
  ;;                       ("emacs" . ?e)
  ;;                       ))
  )

(defun echo/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . echo/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(use-package org-journal
  :disabled
  :after (org)
  :bind (("C-c C-j" . org-journal-new-entry)
         )
  :custom
  (org-journal-dir "~/projects/journal")
  (org-journal-file-type 'weekly)
  ;;(org-journal-start-on-weekday ?)
  (org-journal-file-format "%F.org")
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-time-format "%I:%M %p")
  (org-journal-enable-agenda-integration t))

(defun my-old-carryover (old_carryover)
  (save-excursion
    (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
      (dolist (entry (reverse old_carryover))
        (save-restriction
          (narrow-to-region (car entry) (cadr entry))
          (goto-char (point-min))
          (org-scan-tags '(lambda ()
                            (org-set-tags ":carried:"))
                         matcher org--matcher-tags-todo-only))))))
(setq org-journal-handle-old-carryover 'my-old-carryover)

(use-package org-super-agenda
  :after (org)
  :config
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
                                        ;org-agenda-block-separator nil
                                        ;org-agenda-compact-blocks t
                                        ;org-agenda-start-with-log-mode t
        )
  (setq org-super-agenda-groups
        '(
          (:name "Inbox"
                 :file-path "inbox\.org")
          (:name "Emacs"
                 :tag "emacs")
          (:name "Today"
                 :time-grid t
                 :scheduled today)
          (:name "Due today"
                 :deadline today)
          (:name "Important"
                 :priority "A")
          (:name "Overdue"
                 :deadline past)
          (:name "Due soon"
                 :deadline future)
          (:name "Waiting"
                 :todo "WAIT")
          ))
  (org-super-agenda-mode)
  )

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))

(defun echo/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/projects/settings/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'echo/org-babel-tangle-config))
)

(use-package org-contrib
  :after org
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  )

(use-package denote
  :init
  (denote-rename-buffer-mode 1)
  :config
  (setq denote-directory (expand-file-name "~/projects/docs"))
  (setq denote-known-keywords '("emacs" "food" "bible" "prayer" "encouragement"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords)) ; subdirectory and date are avail
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  (setq denote-date-format nil)

  (setq denote-backlinks-show-context t)

  (require 'denote-journal-extras)
  (setq denote-journal-extras-title-format 'day-date-month-year)

  :hook
  (dired-mode . denote-dired-mode)
  :bind
  ("C-c n n" . denote)
  ("C-c n j" . denote-journal-extras-new-or-existing-entry)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)
  )

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line))
  )

(use-package ws-butler
  :diminish
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode-hook . aggressive-indent-mode)
         (css-mode-hook . aggressive-indent-mode)))

(use-package transient)

(use-package magit
  :after (nerd-icons,transient)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories '(("~/projects" . 1)))
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident ())
          ("Version" 25 magit-repolist-column-version ())
          ("D"        1 magit-repolist-column-flag ())
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U"      3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path"    99 magit-repolist-column-path ())))
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18))
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  )

(use-package magit-todos
  :init
  (magit-todos-mode)
  )

;;(use-package git-timemachine)

(use-package web-mode
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.php[s34]?\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.html.j2\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-indentation nil)
  )

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(setq web-mode-engines-alist
      '(
        ("django" . "/home/echo/projects/website/templates/.*\\.twig.html\\'")
        ("smarty" . "/home/echo/projects/website/templates/.*\\.html\\'")
        )
      )

(use-package rust-mode
  :init
  ;;(setq rust-mode-treesitter-derive t)
  :mode ("\\.rs\\'" . rust-mode))
(use-package rustic
  :after (rust-mode)
  :config
  (setq rustic-format-on-save t))

(use-package python-pytest)
(global-set-key (kbd "C-x T") 'python-pytest-dispatch)

(use-package treesit-auto
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 5)
  (treesit-auto-install 'prompt)
  (treesit-language-source-alist
   '(
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     ))
  (treesit-auto-langs '(javascript yaml json html css elisp php rust))
  :config
  (global-treesit-auto-mode))

(use-package tree-sitter
  :diminish
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

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
  ;; lsp-enable-file-watchers and lsp-file-watch-threshold
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
        lsp-file-watch-threshold 5000)
  :hook (
         (prog-mode-hook . lsp)
         (web-mode . lsp)
         (css-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (echo-install-lsp-servers `(ansible-ls html-ls ts-ls json-ls css-ls iph))
  )

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

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

(use-package projectile
  :diminish projectile-mode ;;TODO: might want to change this
  :config
  (projectile-mode)
  :bind
  ("C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  )

(use-package ripgrep)

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
  :init
  (setq dired-auto-revert-buffer t)
  )
(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

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

(use-package wttrin
  :config
  (setq wttrin-default-cities '("48638"))
  )

(setq tramp-default-method "ssh")

;; (use-package corfu
;;   :init
;;   (global-corfu-mode)
;;   )
(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package free-keys)
;; (use-package bind-key)

(use-package shell-maker
  ;;:straight (:type git :host github :repo "xenodium/shell-maker" :files ("shell-maker*.el"))
  )

(use-package chatgpt-shell
  ;;:straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  ;;:custom
  ;; ((chatgpt-shell-anthropic-key
  ;;   (lambda ()
  ;;     (auth-source-pass-get 'secret "openai-key"))))
  )

(setq mode-line-format
      '("%e"
        (:propertize
         ("" mode-line-mule-info mode-line-client mode-line-modified
          mode-line-remote)
         display (min-width (5.0)))
        " "
        mode-line-buffer-identification
        mode-line-position
        (vc-mode vc-mode)
        mode-line-modes
        mode-line-misc-info
        ))

;;use mode-line-format-right-align, new in 30.1
(use-package time
  :ensure nil
  :config
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  (display-time)
  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq mode-line-end-spaces (list (propertize " "
                                               'display '(space :align-to (- right 6)))
                                   'display-time-string))
  )

(use-package dashboard
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline))
  ;;(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  )

(use-package kkp
  :config
  (global-kkp-mode 1))

(defun pinentry-emacs (desc prompt ok error)
      (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
        str))

(setenv "PAGER" "cat")

(use-package eat
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  )

(use-package eshell
  :ensure nil
  :commands eshell
  :init
  (setq ;; eshell-directory-name (concat cpm-local-dir "eshell/")
   ;; eshell-history-file-name (concat cpm-local-dir "eshell/history")
   ;; eshell-aliases-file (concat cpm-local-dir "eshell/alias")
   ;; eshell-last-dir-ring-file-name (concat cpm-local-dir "eshell/lastdir")
   eshell-highlight-prompt nil
   eshell-buffer-shorthand t
   eshell-cmpl-ignore-case t
   eshell-cmpl-cycle-completions t
   eshell-destroy-buffer-when-process-dies t
   eshell-history-size 10000
   ;; auto truncate after 20k lines
   eshell-buffer-maximum-lines 20000
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-glob-case-insensitive t
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all
   eshell-list-files-after-cd t
   eshell-banner-message ""
   )
  ;; Visual commands
  (setq eshell-visual-commands '("top" "less" "more" "top" "htop" "ssh" "tail"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")

                              (eshell/alias "gd" "magit-diff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "d" "dired $1")

                              (eshell/alias "ll" "ls -AlohG --color=always")))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(with-eval-after-load 'eshell
  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (defun pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))


  (defun pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
      (if (> (length p-lst) 2)
          (concat
           (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                      (substring elm 0 1)))
                      (butlast p-lst 2)
                      "/")
           "/"
           (mapconcat (lambda (elm) elm)
                      (last p-lst 2)
                      "/"))
        pwd)))  ;; Otherwise, we just return the PWD

  (esh-section esh-dir
               ""  ;  (faicon folder)
               (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))
               '(:foreground "#268bd2" :underline t))

  (esh-section esh-git
               "\xe907"  ;  (git icon)
               (with-eval-after-load 'magit
                 (magit-get-current-branch))
               '(:foreground "#b58900"))

  (esh-section esh-python
               "\xe928"  ;  (python icon)
               (with-eval-after-load "virtualenvwrapper"
                 venv-current-name))

  (esh-section esh-clock
               ""  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Separator between esh-sections
  (setq esh-sep " | ")  ; or "  "

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim " ")

  ;; Eshell prompt header
  (setq esh-header "\n ")  ; or "\n "

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp "^>> ") ;; note the '^' to get regex working right
  (setq eshell-prompt-string ">> ")

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-clock))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (
   ("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("M-g M-g" . consult-goto-line)
   ("C-s" . consult-line)
   ("C-f" . consult-imenu))
  :config
  (consult-customize
   consult-theme :preview-key 'any
   consult-line :prompt "Search: " :preview-key 'any
   consult--source-buffer :hidden t :default nil)

  ;;(setq consult-project-root-function #'projectile-project-root)
  ;;(add-to-list 'consult-buffer-sources persp-consult-source)
  )

(use-package embark
  :bind
  (("C-\\" . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :hook
  (elpaca-after-init . global-corfu-mode)
  :bind
  (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package bm
  :bind
  ("<C-left>" . bm-toggle)
  ("<C-up>" . bm-previous)
  ("<C-down>" . bm-next)
  )

(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete)
  :config
  (setq switch-window-minibuffer-shortcut ?z)
  (setq switch-window-shortcut-appearance 'asciiart)
  )
; maybe use winmode instead of switch-window?

(winner-mode)

(use-package zoom
  :diminish
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-mode t)
  )

(use-package buffer-move)

;; (use-package hydra)
;; (defhydra hydra-mywindow ()
;;   "
;;   ^Change Window^   ^Buffer Move^      ^Window^         ^Resize Window^
;;   -------------------------------------------
;;       ↑     	        C-↑             Split _v_ertical    _<prior>_ Enlarge Horizontally
;;       ↓     	        C-↓             Split _h_orizontal  _<next>_ Shrink Horizontally
;;       ←     	        C-←             _k_ill              _<deletechar>_ Shrink Vertically
;;       →               C-→             _u_ndo
;;   _SPC_ cancel
;;   "
;;   ("<up>" windmove-up)
;;   ("<down>" windmove-down)
;;   ("<left>" windmove-left)
;;   ("<right>" windmove-right)
;;   ("C-<up>" buf-move-up)
;;   ("C-<down>" buf-move-down)
;;   ("C-<left>" buf-move-left)
;;   ("C-<right>" buf-move-right)
;;   ("v" split-window-right)
;;   ("h" split-window-below)
;;   ("k" delete-window)
;;   ("u" winner-undo)
;;   ("<prior>" enlarge-window-horizontally)
;;   ("<next>" shrink-window-horizontally)
;;   ("<deletechar>" shrink-window)
;;   ("SPC" nil)
;;   ("q" nil)
;;   )
;; (global-set-key (kbd "C-M-w") 'hydra-mywindow/body)
;; (defhydra hydra-straight-helper (:hint nil)
;;   "
;; _c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
;; _C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
;;  ----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
;; _r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
;; _R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
;;   ("c" straight-check-all)
;;   ("C" straight-check-package)
;;   ("r" straight-rebuild-all)
;;   ("R" straight-rebuild-package)
;;   ("f" straight-fetch-all)
;;   ("F" straight-fetch-package)
;;   ("p" straight-pull-all)
;;   ("P" straight-pull-package)
;;   ("m" straight-merge-all)
;;   ("M" straight-merge-package)
;;   ("n" straight-normalize-all)
;;   ("N" straight-normalize-package)
;;   ("u" straight-push-all)
;;   ("U" straight-push-package)
;;   ("v" straight-freeze-versions)
;;   ("V" straight-thaw-versions)
;;   ("w" straight-watcher-start)
;;   ("W" straight-watcher-quit)
;;   ("g" straight-get-recipe)
;;   ("e" straight-prune-build)
;;   ("q" nil))

(when (string= (system-name) "officedev")
  ;; set the same env as activate_env.sh
  (setenv "ANSIBLE_VAULT_PASSWORD_FILE" "lpass_vault.sh")
  (setenv "ANSIBLE_PIPELINING" nil)
  (setenv "LPASS_DISABLE_PINENTRY" nil)
  (setenv "LPASS_PINENTRY" "~/tools/pinentry-emacs") ;; need to manually install pinentry-emacs
  
  ;; eww, works but is too slow
  ;; (setq projectile-indexing-method 'native)
  (message "'Work' system changes loaded")
  )

(when (string= (system-name) "echo-bedroom")
  ;; not working:
  ;; (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  ;; (setq mode-line-format
  ;;     '("%e"
  ;;       (:propertize
  ;;        ("" mode-line-mule-info mode-line-client mode-line-modified
  ;;         mode-line-remote)
  ;;        display (min-width (5.0)))
  ;;       " "
  ;;       mode-line-buffer-identification
  ;;       mode-line-position
  ;;       projectile-default-mode-line
  ;;       (vc-mode vc-mode)
  ;;       mode-line-modes
  ;;       mode-line-misc-info
  ;;       ))


  (setq browse-url-browser-function 'eww-browse-url)
  (use-package elfeed
    :bind ("C-c f" . elfeed)
    :commands elfeed
    :config
    (setq elfeed-feeds
          '(
            ("https://sachachua.com/blog/category/emacs-news/feed/index.xml" emacs)
            ("https://archlinux.org/feeds/news/" linux)
            ("http://rss.slashdot.org/Slashdot/slashdotLinux")
          ))
    )

  (use-package mastodon
    :config
    (setq mastodon-instance-url "https://techhub.social"
          mastodon-active-user "oshecho")
    )

  (message "'Home desktop' system changes loaded")
  )
