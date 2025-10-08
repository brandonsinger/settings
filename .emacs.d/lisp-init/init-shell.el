;;; init-shell.el --- Setup shell stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setenv "PAGER" "cat")

;; TODO: move this to a better location?
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(require 'em-tramp) ; to load eshell’s sudo

(use-package eat
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

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
   eshell-prefer-lisp-functions t
   eshell-prefer-lisp-variables t
   )
  ;; Visual commands
  (setq eshell-visual-commands '("top" "less" "more" "top" "htop" "ssh" "tail"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")

                              (eshell/alias "sudo" "eshell/sudo $*")

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

(provide 'init-shell)
;;; init-shell.el ends here
