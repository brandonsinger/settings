;;*****************************************************************************
;; **General/Main Settings**
;;*****************************************************************************
(setq inhibit-startup-message   t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq split-width-threshold most-positive-fixnum)
(show-paren-mode 1)

(setq warning-minimum-level :error)

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

(load "php-mode")
(add-to-list 'auto-mode-alist
     	     '("\\.php'" . php-mode))


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

(global-set-key (kbd "C-t") 'test-case-run)


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
;;(setq linum-format "%4d\u2502")
;;linum-mode

(set-face-attribute 'default nil :font "DejaVu Sans Mono-12")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'echo t)

;;(require `tramp)


;;*****************************************************************************
;; **Custom Functions**
;;*****************************************************************************

;; I know that string is in my Emacs somewhere! (http://stackoverflow.com/questions/2641211/emacs-interactively-search-open-buffers)
(require 'cl)
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

(global-set-key [f7] 'search-all-buffers)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-buffer-persistence nil)
 '(bm-highlight-style (quote bm-highlight-line-and-fringe))
 '(custom-safe-themes (quote ("686d1ccaf211e0a0137436189b618f7b2c6bc8af8b8d470dda5bba3c379ede1e" "d3cb966afe2ba576766e99ea84d577b0df82bce3922379a01d702e17788e7a81" "d4bc75a8a8c49e659e1af3028ae68b470b4746733d6b3abb63019ed4d856bb7c" "71f235187b439da113fc5c67b703ff2e1135244566a3c446284180f2053f1e91" "1d73dd76ef3b22a7bfcdeb24410003bc904b814ff98b2ae6b262447b4da0dc30" "82b39200a25929b0d1e5cbdc985ea70e2091a6d247d35ae2e1b54cc55e05505b" "36c7cd3b0c072f7b60fbe5baea40f9586838eddb153a9d0e505293e68e283a67" "2af7ea3af2e1614eee4d7183086266f44544d6c6e08d4ad46babd429d9e03f40" "0142808c139a2632460c6c565abaf15eb56196b11a4f18648573a2272ecb9393" "af3c3bb2af278cf726be8c76f00b147b0225484fc8d5b30440b470d588193457" "ef8334b6aaa9556a8eb0a20c2e93fa7f748033c6de6e91c12e4f0a2c579f6524" "57fd558dfb773856a2a918cef533306f25bef1b40015cac123b5a58cee514f24" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "746b83f9281c7d7e34635ea32a8ffa374cd8e83f438b13d9cc7f5d14dc826d56" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "617219c11282b84761477059b9339da78ce392c974d9308535ee4ec8c0770bee" "854dc57026d3226addcc46b2b460034a74609edbd9c14e626769ac724b10fcf5" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(test-case-phpunit-arguments "--configuration /home/echo/projects/website/test/phpunit.xml"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "gray20"))))
 '(bm-fringe-face ((t (:background "DarkOrange1" :foreground "Black"))))
 '(magit-item-highlight ((t nil))))


