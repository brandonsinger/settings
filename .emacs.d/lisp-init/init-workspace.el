;;; init-workspace.el --- Setup for what I use for 'workspaces' -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; AI generated this chunk
(defun activities-local-buffer-p (buffer)
  "Return non-nil if BUFFER belongs to the current activity."
  (when (activities-current)
    (memq buffer (activities-tabs--tab-parameter
                  'activities-buffer-list
                  (activities-tabs--tab (activities-current))))))
(defvar my-consult--source-activities-buffer
  `(:name     "Activities Buffers"
              :narrow   ?a
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :default  t
              :items    ,(lambda () (consult--buffer-query
                                     :predicate #'activities-local-buffer-p
                                     :sort 'visibility
                                     :as #'buffer-name))))
;; Optional: only show all buffers when NOT in an activity
(defvar my-consult--source-all-buffers
  `(:name     "All Buffers"
              :narrow   ?b
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :enabled  ,(lambda () (not (activities-current)))
              :items    ,(lambda () (consult--buffer-query
                                     :sort 'visibility
                                     :as #'consult--buffer-pair))))


;; TODO: configure/customize some more
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend) ;; saves and removes tab
   ("C-x C-a C-k" . activities-kill)    ;; just removes tab
   ("C-x C-a r" . activities-rename)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)
   ("C-x C-a <DELETE>" . activities-discard) ;; full remove of activity
   ("C-x C-a <left>" . tab-bar-switch-to-prev-tab)
   ("C-x C-a <right>" . tab-bar-switch-to-next-tab)
   ("C-x C-a C-<left>" . my/tab-bar-move-left)
   ("C-x C-a C-<right>" . my/tab-bar-move-right)
   ("C-x b" . consult-buffer)
   )
  :config
  (defvar-keymap activities-repeat-map
    :repeat t
    "<left>"    #'tab-bar-switch-to-prev-tab
    "<right>"   #'tab-bar-switch-to-next-tab
    "C-<left>"  #'my/tab-bar-move-left
    "C-<right>" #'my/tab-bar-move-right)

  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'my-consult--source-all-buffers)
    (add-to-list 'consult-buffer-sources 'my-consult--source-activities-buffer))

  )

(defun my/close-current-tab ()
  "Close the current tab after asking for confirmation."
  (interactive)
  (when (y-or-n-p "Close this tab? ")
    (tab-bar-close-tab)))
(defun my/new-tab-with-scratch (tab-name)
  "Create a new tab named TAB-NAME with *scratch* buffer."
  (interactive "sTab name: ")
  (tab-new)
  (tab-rename tab-name)
  (switch-to-buffer "*scratch*"))
(defun my/projectile-new-tab-scratch ()
  "Select a projectile project, open it in a new tab, and switch to *scratch* buffer."
  (interactive)
  (when (not (boundp 'projectile-known-projects))
    (projectile-discover-projects-in-search-path))
  (let* ((project (completing-read "Switch to project: "
                                   projectile-known-projects))
         (project-name (file-name-nondirectory (directory-file-name project))))
    (tab-bar-new-tab)
    (tab-bar-rename-tab (concat "Proj: " project-name)
                        (let ((default-directory project))
                          (switch-to-buffer "*scratch*")))))
(defun my/tab-bar-move-left ()
  "Move current tab to the left."
  (interactive)
  (tab-bar-move-tab -1))
(defun my/tab-bar-move-right ()
  "Move current tab to the right."
  (interactive)
  (tab-bar-move-tab 1))

(use-package tab-bar
  :ensure nil
  :bind
  (
   ("C-x t <left>" . tab-bar-switch-to-prev-tab)
   ("C-x t <right>" . tab-bar-switch-to-next-tab)
   ("C-x t C-<left>" . my/tab-bar-move-left)
   ("C-x t C-<right>" . my/tab-bar-move-right)
   ("C-x t R" . tab-bar-rename-tab)
   ("C-x t K" . my/close-current-tab)
   ("C-x t N" . my/new-tab-with-scratch)
   ("C-x t P" . my/projectile-new-tab-scratch)
   ))

(provide 'init-workspace)
;;; init-workspace.el ends here
