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


;(if window-system
 ;(size-screen)
;)



(load "php-mode")



(setq c-default-style "python")
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode t)))


;;(global-set-key "\C-<TAB>" '\t)


(require 'egg)

;(require 'lusty-explorer)
;(global-set-key (kbd "\C-x \C-f") 'lusty-file-explorer)
;(global-set-key (kbd "\C-x \C-b") 'lusty-buffer-explorer)


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

;; For bookmarking
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)


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
(require 'color-theme-tango)
(if window-system
  (color-theme-tango))


(require 'icicles)