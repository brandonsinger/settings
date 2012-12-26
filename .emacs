;;*****************************************************************************
;; **General/Main Settings**
;;*****************************************************************************
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


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")  ; Add this directory to Emacs' load path

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)





;;*****************************************************************************
;; **Editing Modes Stuff**
;;*****************************************************************************

(load "~/.emacs.d/site-lisp/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 2)

(load "~/.emacs.d/site-lisp/jinja.el")

;(if window-system
 ;(size-screen)
;)

(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)


;;*****************************************************************************
;; **Tab Settings**
;;*****************************************************************************
(setq c-default-style "python")
(setq-default c-basic-offset 4
              tab-width 4)
(add-hook 'python-mode-hook
	  (lambda()
	    (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
	    ))
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


;;*****************************************************************************
;; **Custom Keys**
;;*****************************************************************************
(global-set-key (kbd "C-`") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer) ;;open status in full window, dont split

(iswitchb-mode 1) ;;Change what happens on C-x b
(global-set-key (kbd "C-x C-b") 'bs-show)

;(defun my-compile ()
;  "Use compile to run python programs"
;  (interactive)
;  (compile (concat "python " (buffer-name))))
;(setq compilation-scroll-output t)
;(global-set-key "\C-c\C-c" 'my-compile)



;;*****************************************************************************
;; **Editing options**
;;*****************************************************************************
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


;;*****************************************************************************
;; **For bookmarking**
;;*****************************************************************************
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "[D") 'bm-toggle) ;ctrl-left through screen
(global-set-key (kbd "[A") 'bm-previous) ;ctrl-up through screen
(global-set-key (kbd "[B") 'bm-next) ;ctrl-down through screen
(global-set-key (kbd "<C-left>") 'bm-toggle) ;ctrl-left
(global-set-key (kbd "<C-up>") 'bm-previous) ;ctrl-up
(global-set-key (kbd "<C-down>") 'bm-next) ;ctrl-down
(global-set-key "\M-[1;5D" 'bm-toggle) ;ctrl-left through tmux
(global-set-key "\M-[1;5A" 'bm-previous) ;ctrl-up through tmux
(global-set-key "\M-[1;5B" 'bm-next) ;ctrl-down through tmux


;;*****************************************************************************
;; **Visuals**
;;*****************************************************************************

;;(require 'linum)
;;(global-linum-mode 1)
;;nlinum-mode?

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-tomorrow-night t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
