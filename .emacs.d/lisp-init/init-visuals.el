;;; init-visuals.el --- Some visual stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt visible-bell t)

(defun my/change-cursor-shape ()
  "Set cursor shape depending on overwrite mode.
When `overwrite-mode' is nil (off), set `cursor-type' to box.
Otherwise, set it to hbar."
  (interactive)
  (if (not overwrite-mode)
      (setq cursor-type 'box)
    (setq cursor-type 'hbar)))
(add-hook 'overwrite-mode-hook 'my/change-cursor-shape)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                help-mode-hook
                helpful-mode-hook
                ripgrep-search-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package tab-line
  :ensure nil
  :config
  ;; TODO: use tab-bar-format instead of tab-bar-new-button-show
  (setq tab-bar-close-button-show (display-graphic-p)
        tab-bar-new-button-show (display-graphic-p)
        tab-line-right-button (propertize " ▶ "
                                          'keymap tab-line-right-map
                                          'mouse-face 'tab-line-highlight
                                          'help-echo "Click to scroll right")
        tab-line-left-button (propertize " ◀ "
                                         'keymap tab-line-left-map
                                         'mouse-face 'tab-line-highlight
                                         'help-echo "Click to scroll left"))

  :custom-face
  (activities-tabs ((t (:height 1.2))))
  (mode-line ((t (:height 1.2)))))

(my-need-linux-package "Roboto Mono")
(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Roboto Mono" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "Roboto Mono"))
(when (member "RobotoMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 108)
  (set-face-attribute 'fixed-pitch nil :family "RobotoMono Nerd Font"))

(global-hl-line-mode 1)
(add-hook 'magit-mode-hook
          (lambda ()
            (make-local-variable 'global-hl-line-mode)
            (setq global-hl-line-mode nil)))
(use-package lin
  :hook
  (elpaca-after-init . lin-global-mode)
  :config
  (setq lin-face 'lin-blue))


;; To make this setup work, the user must type M-x and then call the command nerd-icons-install-fonts.
(use-package nerd-icons)

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
;; (use-package nerd-icons-multimodal
;;   :if (display-graphic-p)
;;   :hook
;;   ((archive-mode tar-mode dired-mode) . nerd-icons-multimodal-mode))
;; (use-package nerd-icons-grep
;;   :if (display-graphic-p)
;;   :init
;;   (nerd-icons-grep-mode)
;;   :custom
;;   ;; This setting is a pre-requirement, so an icon can be displayed near each
;;   ;; heading
;;   (grep-use-headings t))

(use-package rainbow-mode
  :hook
  (web-mode-hook . rainbow-mode)
  (css-mode-hook . rainbow-mode)
  (html-mode-hook . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t)))

(use-package dimmer
  :config
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t))

(use-package svg-margin
  :ensure (:host github :repo "chiply/svg-margin")
  :config
  (setq svg-margin-disable-fringe 'left)
  (setq svg-margin-renderer (if (display-graphic-p) 'svg 'text))
  ;; (svg-margin-register-provider 'vc
  ;;                               (lambda (_buffer)
  ;;                                 (list (list :line 10 :shape 'bar :color "#8fb39a" :help "added")))
  ;;                               :side 'left :priority 9)

  (svg-margin-register-provider 'flycheck
                                (lambda (buffer)
                                  (with-current-buffer buffer
                                    (when (bound-and-true-p flycheck-mode)
                                      (cl-loop for err in flycheck-current-errors
                                               collect (list :line (flycheck-error-line err)
                                                             :text (nerd-icons-codicon "nf-cod-bug") :font "Symbols Nerd Font Mono"
                                                             :scale .7
                                                             :side 'left
                                                             :color (pcase (flycheck-error-level err)
                                                                      ('error "#cc3333")
                                                                      ('warning "#cc9933")
                                                                      (_ "#3333cc"))
                                                             :help (flycheck-error-message err))))))
                                :side 'left)

  (svg-margin-register-provider 'jinx
                                (lambda (buffer)
                                  (with-current-buffer buffer
                                    (when (bound-and-true-p jinx-mode)
                                      (cl-loop for ov in (jinx--get-overlays (point-min) (point-max))
                                               collect (list :line (line-number-at-pos (overlay-start ov))
                                                             :shape 'circle
                                                             :side 'left
                                                             :color "#cc9933"
                                                             :help "Spelling error")))))
                                :side 'left)

  ;; (svg-margin-register-provider 'todo
  ;;                               (lambda (_buffer)
  ;;                                 (list (list :line 10 :shape 'dot  :color "#cc3333")
  ;;                                       (list :line 10 :shape 'bar  :color "#3333cc" :column 1)
  ;;                                       (list :line 25 :text "!"    :side 'left :face 'warning))))
  (setq svg-margin-arrangement 'fixed
        svg-margin-provider-columns '((flycheck  . 0)    ; 0 = nearest the text
                                      (jinx  . 1)
                                      ))
  (global-svg-margin-mode)
  (svg-margin-hover-mode 1)
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-after-syntax-check-hook
              (lambda () (svg-margin-refresh (current-buffer)))))
  (with-eval-after-load 'jinx
    (dolist (hook jinx--reschedule-hooks)
      (add-hook hook (lambda (&rest _) (svg-margin-refresh (current-buffer))))))
  
  )

(provide 'init-visuals)
;;; init-visuals.el ends here
