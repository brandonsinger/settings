;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)

(setq-default tab-width 4)

(column-number-mode)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)


(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Set the right directory to store the native comp cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x <") 'org-insert-structure-template)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;;todo: use instead? (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; Bootsrap straight.el
(setq straight-repository-branch "master")
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-compute-statistics t) ;; invoke use-package-report

(use-package diminish)

(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

(use-package no-littering)

(defun echo/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  )
(use-package org
  :delight
  :hook (org-mode . echo/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files '("~/projects/gtd/inbox.org"
                           "~/projects/gtd/gtd.org"
                           "~/projects/gtd/tickler.org"))
  (setq org-capture-templates
        '(
          ("t" "Todo [inbox]" entry
           (file+headline "~/projects/gtd/inbox.org" "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline "~/projects/gtd/tickler.org" "Tickler")
           "* %i%? \n %U")
          ))
  (setq org-refile-targets '(("~/projects/gtd/gtd.org" :maxlevel . 3)
                             ("~/projects/gtd/someday.org" :level . 1)
                             ("~/projects/gtd/tickler.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq-default org-enforce-todo-dependencies t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w)
                        ("@home" . ?h)
                        ("@computer" . ?c)
                        (:endgroup . nil)
                        ("emacs" . ?e)
                        ))
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

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)
         ("<home>" . mwim-beginning-of-code-or-line)
         ("<end>" . mwim-end-of-code-or-line))
  )

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories '(("~/projects" . 1)))
  (setq magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident ())
          ("Version" 25 magit-repolist-column-version ())
          ("D"        1 magit-repolist-column-dirty ())
          ("B<U"      3 magit-repolist-column-unpulled-from-upstream
           ((:right-align t)
            (:help-echo "Upstream changes not in branch")))
          ("B>U"      3 magit-repolist-column-unpushed-to-upstream
           ((:right-align t)
            (:help-echo "Local changes not in upstream")))
          ("Path"    99 magit-repolist-column-path ())))
  )

(use-package magit-todos
  :init
  (magit-todos-mode)
  )

;;(use-package git-timemachine)

(use-package web-mode
  :config
  (setq web-mode-enable-auto-indentation nil)
  )

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(use-package python-mode
  :ensure nil
  :custom
  (python-shell-interperter "python")
  )

;; Todo
(require 'web-mode)
(setq web-mode-enable-auto-indentation nil)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php[s34]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.j2\\'" . web-mode))

(setq web-mode-engines-alist
      '(
        ("smarty" . "/home/echo/projects/website/templates/.*\\.html\\'")
        )
      )

(use-package python-pytest)
(global-set-key (kbd "C-x T") 'python-pytest-dispatch)

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
  :diminish projectile-mode
  :config
  (projectile-mode)
  :bind
  ("C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  )

(use-package ripgrep)

(use-package perspective
  :straight t
  :bind
  ("C-x k" . persp-kill-buffer*)
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x C-b" . persp-switch-to-buffer*)
  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  (persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
  (persp-sort 'created)
  :init
  (persp-mode)
  (persp-state-load persp-state-default-file)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  )

(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  )

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(use-package devdocs
  :bind
  ("C-h D" . devdocs-lookup)
  )

;; (straight-use-package
;;  '(eat :type git
;;        :host codeberg
;;        :repo "akib/emacs-eat"
;;        :files ("*.el" ("term" "term/*.el") "*.texi"
;;                "*.ti" ("terminfo/e" "terminfo/e/*")
;;                ("terminfo/65" "terminfo/65/*")
;;                ("integration" "integration/*")
;;                (:exclude ".dir-locals.el" "*-tests.el"))))

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

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  )

(use-package wttrin
  :config
  (setq wttrin-default-cities '("48638"))
  )

(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package savehist
  :straight nil
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
  :bind
  (("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("M-g M-g" . consult-goto-line)
   ("C-s" . consult-line)
   ("C-f" . consult-imenu))
  :config
  (consult-customize
   consult-theme :preview-key 'any
   consult-line :prompt "Search: " :preview-key 'any
   consult--source-buffer :hidden t :default nil)

  (setq consult-project-root-function #'projectile-project-root)
  (add-to-list 'consult-buffer-sources persp-consult-source))


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
  :init
  (global-corfu-mode)
  )

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

(use-package hydra)
(defhydra hydra-mywindow ()
  "
  ^Change Window^   ^Buffer Move^      ^Window^         ^Resize Window^
  -------------------------------------------
      ↑     	        C-↑             Split _v_ertical    _<prior>_ Enlarge Horizontally
      ↓     	        C-↓             Split _h_orizontal  _<next>_ Shrink Horizontally
      ←     	        C-←             _k_ill              _<deletechar>_ Shrink Vertically
      →               C-→             _u_ndo
  _SPC_ cancel
  "
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("C-<up>" buf-move-up)
  ("C-<down>" buf-move-down)
  ("C-<left>" buf-move-left)
  ("C-<right>" buf-move-right)
  ("v" split-window-right)
  ("h" split-window-below)
  ("k" delete-window)
  ("u" winner-undo)
  ("<prior>" enlarge-window-horizontally)
  ("<next>" shrink-window-horizontally)
  ("<deletechar>" shrink-window)
  ("SPC" nil)
  ("q" nil)
  )
(global-set-key (kbd "C-M-w") 'hydra-mywindow/body)

(setq visible-bell t)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook)
              )
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq mode-line-format
      '("%e" mode-line-client mode-line-modified " " mode-line-buffer-identification  mode-line-position (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

(use-package eldoc
  :straight nil
  :diminish)

(use-package modus-themes
  :disabled
  :demand t
  :after (org)
  :init
  (setq modus-themes-mode-line '(accented borderless padded))
  (setq modus-themes-region '(bg-only))
  (setq modus-themes-completion 'opinionated)
  (setq modus-themes-paren-match '(bold intense))
  (setq modus-themes-syntax '(yellow-comments))
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-headings
        '((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))
  (setq modus-themes-scale-headings t)
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-subtle-line-numbers t)
  :config
  (load-theme 'modus-vivendi t)
  :bind
  ("<f5>" . modus-themes-toggle)
  )

(use-package ef-themes
  :demand t
  :after (org)
  :init
  (setq ef-themes-to-toggle '(ef-bio ef-day))


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

(use-package rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

(use-package prism
  )


(use-package beacon
  :diminish
  :config
  (beacon-mode 1))
