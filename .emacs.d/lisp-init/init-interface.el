;;; init-interface.el --- ??? -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; for minibuffer completions
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("TAB" . minibuffer-complete)
        ("<prior>" . vertico-scroll-down)
        ("<next>" . vertico-scroll-up)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package dl-completion
  :ensure (:host codeberg :repo "Hyudoro/dl-completion")
  :after orderless
  :config
  (setq completion-styles '(orderless basic dl-completion)))

(use-package prescient)
(use-package corfu-prescient
  :after (precsient corfu))
(use-package vertico-prescient
  :after (prescient vertico))

;; TODO: move this comment somewhere better?
;; To get super-tab working in xfce:
;;  xfconf-query -c xfce4-keyboard-shortcuts -p "/xfwm4/custom/<Super>Tab" --reset

(use-package cape
  :bind
  ("C-c p" . cape-prefix-map)
  ("s-<tab>" . completion-at-point)
  ("C-c <tab>" . completions-at-point)
  :init
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev) TODO: use once I start using abbreviations
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ; Complete word from current buffers.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dict))

;; These functions from https://utcc.utoronto.ca/~cks/space/blog/programming/EmacsSwitchingToOnlyCorfu
(defun corfu-enable-auto ()
  "Enable corfu auto-completion in this buffer."
  (interactive)
  (setq-local corfu-auto t)
  (corfu-mode -1)
  (corfu-mode 1))
(defun corfu-disable-auto ()
  "Disable corfu auto-completion in this buffer."
  (interactive)
  (setq-local corfu-auto nil)
  (corfu-mode -1)
  (corfu-mode 1))

;; for in-line completions
(use-package corfu
  :hook
  (elpaca-after-init . global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(2 . .5)))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; TODO: reorganize this chunk?

(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(add-hook 'conf-mode-hook #'completion-preview-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2)

  ;; Non-standard commands to that should show the preview:

  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)

  ;; Bindings that take effect when the preview is shown:

  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

(defvar my-consult-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'previous-history-element)
    map))
(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

;; Remember: "At a command prompt type M-n and typically Consult will insert the symbol or thing at point into the input."
(use-package consult
  :after projectile
  :init
  (recentf-mode 1)
  :bind
  (
   ("C-x b" . consult-buffer)
   ("M-g M-g" . consult-goto-line)
   ("C-s" . consult-line)
   ("C-f" . consult-imenu)
   ("M-s ." . consult-line-thing-at-point))
  ;; TODO: bind consult-outline to something
  :config
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-theme :preview-key 'any
   consult-line :prompt "Search: " :preview-key 'any :add-history (seq-some #'thing-at-point '(region symbol)) :keymap my-consult-line-map
   consult-line-thing-at-point :initial (thing-at-point 'symbol)
   consult-source-buffer :hidden t :default nil)
  (setq consult-project-root-function #'projectile-project-root)

  ;; Configure initial narrowing per command
  (defvar consult-initial-narrow-config
    '((consult-buffer . ?p)))
  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 250))

;; TODO: figure out a good keybinding for this
(use-package consult-todo
  :after (consult))

;; TODO: test this out in a rust project? then determine to remove it or not.
(use-package consult-lsp
  :after (consult))

(use-package consult-flycheck
  :after (consult)
  :bind
  ("C-c E" . consult-flycheck))

(use-package embark
  :bind
  (("C-\\" . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Using embark-prefix-help-command instead of which-key
  (setq prefix-help-command #'embark-prefix-help-command)

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

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; TODO: switch to bookmarks? bookmarks+?
(use-package bm
  :bind
  ("<C-left>" . bm-toggle)
  ("<C-up>" . bm-previous)
  ("<C-down>" . bm-next)
  ;; TODO: try using these instead?
  ("<f2>" . bm-next)
  ("S-<f2>" . bm-previous)
  ("C-<f2>" . bm-toggle)
  :config
  (if (display-graphic-p)
      (progn (setopt bm-highlight-style 'bm-highlight-only-fringe)
             (setopt bm-marker 'bm-marker-right))
    (setopt bm-highlight-style 'bm-highlight-only-line)))

(use-package smartparens
  :hook
  ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-use-C-h-commands nil))

(setq mouse-yank-at-point t)

(use-package super-save
  :ensure t
  :diminish
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (super-save-mode +1))

;; treesit-fold setup in other file
(use-package kirigami
  :bind
  (("C-c z o" . kirigami-open-fold)          ; Open fold at point
   ("C-c z O" . kirigami-open-fold-rec)      ; Open fold recursively
   ("C-c z e" . kirigami-open-folds)         ; Expand all folds
   ("C-c z c" . kirigami-close-fold)         ; Close fold at point
   ("C-c z z" . kirigami-close-folds)        ; Close all folds
   ("C-c C-z" . kirigami-toggle-fold)        ; Toggle fold at point
   ("s-z" . kirigami-toggle-fold))
  :custom
  (kirigami-show-context-menu t)
  :config
  (kirigami-global-mode 1))

(provide 'init-interface)
;;; init-interface.el ends here
