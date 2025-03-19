;;; init-window.el --- Setup window stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete)
  :config
  (setq switch-window-minibuffer-shortcut ?z)
  (setq switch-window-shortcut-appearance 'asciiart)
  )
;; maybe use windmode instead of switch-window? or ace-window

(winner-mode)

(use-package zoom
  :diminish
  :init  
  (zoom-mode t)
  (zoom-size '(0.618 . 0.618)))

(use-package buffer-move)

(use-package hydra
  :config
  (defhydra hydra-mywindow ()
    "
    ^Change Window^   ^Buffer Move^      ^Window^         ^Resize Window^
    -------------------------------------------
        ↑     	        C-↑             Split _v_ertical    _<prior>_ Enlarge Horizontally
        ↓     	        C-↓             Split _h_orizontal  _<next>_ Shrink Horizontally
        ←     	        C-←             _k_ill              _<deletechar>_ Shrink Vertically
        →               C-→             _u_ndo
    _SPC_ cancel
    "
    ("<up>" windmove-up)
    ("<down>" windmove-down)
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("C-<up>" buf-move-up)
    ("C-<down>" buf-move-down)
    ("C-<left>" buf-move-left)
    ("C-<right>" buf-move-right)
    ("v" split-window-right)
    ("h" split-window-below)
    ("k" delete-window)
    ("u" winner-undo)
    ("<prior>" enlarge-window-horizontally)
    ("<next>" shrink-window-horizontally)
    ("<deletechar>" shrink-window)
    ("SPC" nil)
    ("q" nil)
    )
  (global-set-key (kbd "C-M-w") 'hydra-mywindow/body))


(provide 'init-window)
;;; init-window.el ends here
