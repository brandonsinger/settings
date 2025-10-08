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
  (completion-category-overrides '((file (styles . (basic partial-completion orderless))))))

;; for in-line completions
(use-package corfu
  :hook
  (elpaca-after-init . global-corfu-mode)
  :bind
  (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :custom
  (corfu-cycle t)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(5 . 1)))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

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
   ;; ("C-x b" . consult-buffer)
   ;; ("C-x C-b" . consult-buffer)
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
   consult--source-buffer :hidden t :default nil)
  (setq consult-project-root-function #'projectile-project-root)
  (setq read-file-name-function #'consult-find-file-with-preview)

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

(use-package consult-todo
  :after (consult))

;; TODO: test this out in a rest project? then determine to remove it or not.
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
  :ensure t
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 3)
  (which-key-use-C-h-commands nil))

(provide 'init-interface)
;;; init-interface.el ends here
