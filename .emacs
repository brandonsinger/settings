(setq inhibit-startup-message   t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq split-width-threshold most-positive-fixnum)
(show-paren-mode 1)

(desktop-save-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)


(add-to-list 'load-path "~/.emacs.d/site-lisp/")  ; Add this directory to Emacs' load path

(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 2)

(load "~/.emacs.d/site-lisp/jinja.el")

;(if window-system
 ;(size-screen)
;)


(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)


(setq c-default-style "python")
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))
(add-hook 'nxhtml-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))
(add-hook 'nxml-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))

(setq nxml-child-indent 4)


;;(global-set-key "\C-<TAB>" '\t)


(require 'egg)
;(setq egg-buffer-hide-section-type-on-start true)
;;C-x v i == egg-file-stage-current-file == git add
;;C-x v l == egg-log == git log
;;C-x v u == egg-file-cancel-modifications == unconditionally delete unstaged modifications in the current file. git checkout
;;C-x v b == egg-start-new-branch == git checkout -b new_branch



;;**Editing options**

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)


; Make Emacs use "newline-and-indent" when you hit the Enter key so
; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m"        'newline-and-indent)

(global-set-key (kbd "C-x C-b") 'bs-show)

;; For bookmarking
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "[D") 'bm-toggle) ;ctl-left
(global-set-key (kbd "[A") 'bm-previous) ;ctl-up
(global-set-key (kbd "[B") 'bm-next) ;ctl-down


(global-set-key (kbd "C-`") 'egg-status)

(iswitchb-mode 1) ;Change what happens on C-x b

;(require 'tramp)
;(setq tramp-default-method "pscp")

;(defun my-compile ()
;  "Use compile to run python programs"
;  (interactive)
;  (compile (concat "python " (buffer-name))))
;(setq compilation-scroll-output t)
;(global-set-key "\C-c\C-c" 'my-compile)


;;**Visuals**

(require 'linum)
(global-linum-mode 1)

;; color theme (requires http://www.emacswiki.org/cgi-bin/wiki?ColorTheme )
(require 'color-theme)
(require 'color-theme-echo)
(if window-system
  (color-theme-echo))


;(require 'icicles)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :section)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil))))
