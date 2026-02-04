;;; init-profile-home.el --- Setup some stuff when at home -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar my/ollama-model 'qwen2.5-coder:7b)
(with-eval-after-load 'gptel
  (setq
   gptel-model my/ollama-model
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models (list my/ollama-model))))

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

(setq initial-frame-alist
      '((width . 130)
        (height . 42)
        (left . 1.0)
        (top . 50)))

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
        mastodon-active-user "oshecho"))

(use-package denote
  :disabled
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
  ("C-c n R" . denote-rename-file-using-front-matter))


(setq echo-install-lsp-servers-list `(html-ls js-ls json-ls css-ls python-ls rs-ls))

(message "'Home desktop' system changes loaded")
(provide 'init-profile-home)
;;; init-profile-home.el ends here
